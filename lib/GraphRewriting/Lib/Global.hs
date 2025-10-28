{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module GraphRewriting.Lib.Global where

import Control.Monad (replicateM_)
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import GraphRewriting.Graph
import GraphRewriting.Graph.Read
import GraphRewriting.Pattern
import GraphRewriting.Rule
import Prelude.Unicode
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM, or)

data GlobalVars n = GlobalVars
  { graph :: Graph n -- graph to execute
  , getRules :: RuleTree n -- named tree with rules
  , layoutStep :: Node -> Rewrite n () -- ??the next state of the graph by Node??
  , selectedRule :: Int
  }

data LabelledTree a = Branch String [LabelledTree a] | Leaf String a

data LTZipper a = Root | Child String [LabelledTree a] (LTZipper a) [LabelledTree a]

type LTLoc a = (LabelledTree a, LTZipper a)

type RuleTree n = LabelledTree (Int, Rule n)

-- depth-first traversal
next :: LTLoc a -> Maybe (LTLoc a)
next (Branch b (t : ts), p) = Just (t, Child b [] p ts)
next (Leaf l x, p) = right (Leaf l x, p)
next _ = Nothing

nth :: Int -> LTLoc a -> Maybe (LTLoc a)
nth n l = iterate (>>= next) (Just l) !! n

right :: LTLoc a -> Maybe (LTLoc a)
right (t, Child c ls p (r : rs)) = Just (r, Child c (ls ⧺ [t]) p rs)
right (t, Child c ls p []) = up (t, Child c ls p []) >>= right
right _ = Nothing

up :: LTLoc a -> Maybe (LTLoc a)
up (t, Child c ls p rs) = Just (Branch c (ls ⧺ [t] ⧺ rs), p)
up _ = Nothing

put :: LTLoc a -> LabelledTree a -> LTLoc a
put (_, p) t = (t, p)

top :: LTLoc a -> LTLoc a
top (t, Root) = (t, Root)
top (t, Child c ls p rs) = top (Branch c (ls ⧺ [t] ⧺ rs), p)

root :: LabelledTree a -> LTLoc a
root t = (t, Root)

instance Foldable LabelledTree where
  foldr f y (Leaf l x) = f x y
  foldr f y (Branch l ts) = foldr (flip $ foldr f) y ts

instance Functor LabelledTree where
  fmap f (Leaf l x) = Leaf l (f x)
  fmap f (Branch l ts) = Branch l $ fmap f <$> ts

instance Traversable LabelledTree where
  traverse f (Leaf l x) = Leaf l <$> f x
  traverse f (Branch l ts) = Branch l <$> traverse (traverse f) ts

showRuleTree :: RuleTree n -> String
showRuleTree = showLabelledTree 2 0 (+) . fmap fst

showLabelledTree :: (Show a) => Int -> a -> (a -> a -> a) -> LabelledTree a -> String
showLabelledTree indentation init combine = snd . rec
 where
  rec (Leaf l x) = (x, l ⧺ " " ⧺ show x)
  rec (Branch l ts) = (x, l ⧺ " " ⧺ show x ⧺ "\n" ⧺ indent (unlines ls))
   where
    x = foldr combine init xs
    (xs, ls) = Prelude.unzip $ map rec ts

  indent str = unlines $ map (replicate indentation ' ' ⧺) (lines str)

  unlines [] = ""
  unlines [x] = x
  unlines (x : xs) = x ⧺ "\n" ⧺ unlines xs

instance (Show a) => Show (LabelledTree a) where
  show :: (Show a) => LabelledTree a -> String
  show (Leaf l x) = l ⧺ " " ⧺ show x
  show (Branch l s) = l ⧺ "\n" ⧺ indent (unlines $ map show s)
   where
    indent str = unlines $ map (replicate 2 ' ' ⧺) (lines str)
    unlines [] = ""
    unlines [x] = x
    unlines (x : xs) = x ⧺ "\n" ⧺ unlines xs

readGraph :: IORef (GlobalVars n) -> IO (Graph n)
readGraph = fmap graph . readIORef

writeGraph :: Graph n -> IORef (GlobalVars n) -> IO ()
writeGraph g = modifyGraph (const g)

modifyGraph :: (Graph n -> Graph n) -> IORef (GlobalVars n) -> IO ()
modifyGraph f globalVars = do
  modifyIORef globalVars $ \v -> v{graph = f $ graph v}

highlight :: IORef (GlobalVars n) -> IO (Set Node)
highlight globalVars = do
  gv@GlobalVars{graph = g, getRules = rs, selectedRule = r} <- readIORef globalVars
  let rule = foldMap snd (subtrees rs !! r)
      h = Set.fromList [head match | (match, rewrite) <- runPattern rule g]
  return h

subtrees :: LabelledTree a -> [LabelledTree a]
subtrees t =
  t : case t of
    Leaf _ _ -> []
    Branch l ts -> concatMap subtrees ts

numNodes :: LabelledTree a -> Int
numNodes = length . subtrees

{- | Traverses the rule tree depth-first and executes all leaf rules it encounters. Rules are
executed everywhere they match, except if they overlap one of them is chosen at random.
So this corresponds to a complete development.
-}
applyLeafRules :: (Show n) => (Rule n -> Rule n) -> Int -> IORef (GlobalVars n) -> IO ()
applyLeafRules restriction idx gvs = do
  g <- readGraph gvs

  -- Choose rules by index
  comptree <- getRules <$> readIORef gvs
  let pos = nth idx (root comptree)

  case pos of
    Nothing -> return ()
    -- (KubEF: tree is set of rules to apply)
    Just (tree, p) -> do
      let ns = evalGraph readNodeList g
          -- first we mark all redexes
          rule = restriction $ foldMap snd tree
          -- then we find a non-overlapping subset
          ms = head $ evalPattern (matches rule) g
          -- then we apply the rules in the leafs while restricting them to that subsetT
          ((_, g'), tree') = mapAccumL applyLeafRules' (ms, g) tree
          ns' = evalGraph readNodeList g'
          newNodes = ns' Data.List.\\ ns
      layout <- layoutStep <$> readIORef gvs
      let newGraph = execGraph (replicateM_ 15 (mapM layout newNodes)) g'
      writeGraph newGraph gvs
      modifyIORef gvs $ \x -> x{getRules = fst $ top (tree', p)}
 where
  -- At every leaf apply the rule restricted to the set of predetermined matches, every time removing the
  -- the match from the set updating the graph and the counter.
  -- applyLeafRules' :: ([Match], Graph n) -> (Int, Rule n) -> (([Match], Graph n), (Int, Rule n))
  -- (KubEF: fully unUI subfunction)
  applyLeafRules' (matches, g) (n, r) =
    case runPattern r' g of
      [] -> ((matches, g), (n, r))
      (match, rewrite) : _ -> applyLeafRules' (filter (not . any (`elem` match)) matches, g') (n + 1, r)
       where
        g' = execGraph rewrite g
   where
    r' = restrictOverlap (\past future -> future `elem` matches) (restriction r)
