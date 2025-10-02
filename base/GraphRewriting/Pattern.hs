{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Patterns allow monadic scrutiny of the graph (modifications are not possible) while keeping track of matched nodes (history). A 'Pattern' is interpreted by 'runPattern' that returns a result for each position in the graph where the pattern matches. It is allowed to 'fail' inside the 'Pattern' monad, indicating that the pattern does not match, which corresponds to conditional rewriting.
module GraphRewriting.Pattern (module GraphRewriting.Pattern, PatternT, Pattern, Match, (<|>)) where

import Control.Applicative
import Control.Monad.List
import Control.Monad.Reader
import Data.Functor.Identity
import qualified Data.Set as Set (empty, insert, member)
import GraphRewriting.Graph.Read
import GraphRewriting.Pattern.Internal
import Prelude.Unicode

-- | A pattern represents a graph scrutiny that memorises all the scrutinised nodes during matching.
type Pattern n = PatternT n

instance MonadFail Identity where
  fail = error

instance Monad (PatternT n) where
  return = pure
  p >>= f = PatternT $ \h -> do
    (m1, x) <- patternT p h
    (m2, y) <- patternT (f x) (reverse m1 ⧺ h)
    return (m1 ⧺ m2, y)

instance MonadFail (PatternT n) where
  fail str = PatternT $ \h -> lift (fail str)

-- TODO: Change constraint to Functor m if possible
instance Functor (PatternT n) where fmap = liftM

-- TODO: Change constraint from Monad m if possible
instance Applicative (PatternT n) where
  pure x = PatternT $ \h -> pure ([], x)
  f <*> x = do
    f' <- f
    f' <$> x

instance Alternative (PatternT n) where
  empty = mzero
  (<|>) = mplus

instance Semigroup (PatternT n a) where
  (<>) = mplus

instance Monoid (PatternT n a) where
  mempty = mzero
  mappend = (<>)

instance MonadPlus (PatternT n) where
  mzero = fail "empty result list"
  mplus p q = PatternT $ \h -> do
    -- TODO: this implements choice. Is mplus the right function for that?
    g <- ask
    lift $ runReaderT (patternT p h) g `mplus` runReaderT (patternT q h) g

runPatternT :: PatternT n a -> Graph n -> [(Match, a)]
runPatternT = runPatternT' []

-- | Apply a pattern on a graph returning a result for each matching position in the graph together with the matched nodes.
runPattern :: Pattern n a -> Graph n -> [(Match, a)]
runPattern = runPatternT

evalPattern :: Pattern n a -> Graph n -> [a]
evalPattern p = map snd . runPattern p

execPattern :: Pattern n a -> Graph n -> [Match]
execPattern p = map fst . runPattern p

-- combinators ---------------------------------------------------------------

-- | Something like an implicit monadic map
branch :: [a] -> PatternT n a -- TODO: express this using Alternative?
branch xs = PatternT $ \h -> lift [([], x) | x <- xs]

-- | 'branch' on each node, add it to the history, and return it
branchNodes :: [Node] -> PatternT n Node
branchNodes ns = do
  -- TODO: express this using Alternative?
  n <- branch ns
  visit n
  return n

-- | Probe whether a pattern matches somewhere on the graph. You might want to combine this with 'amnesia'.
probe :: PatternT n a -> PatternT n Bool
probe p = not . null <$> matches p

-- | probe a pattern returning the matches it has on the graph. You might want to combine this with 'amnesia'.
matches :: PatternT n a -> PatternT n [Match]
matches p = map fst <$> match p

-- TODO: isn't this essentially same as runPatternT?

-- | probe a pattern returning the matches it has on the graph. You might want to combine this with 'amnesia'.
match :: PatternT n a -> PatternT n [(Match, a)]
match p =
  PatternT $ \h -> do
    matches <- asks (runReaderT $ patternT p h) -- list of all possible matches
    let roundup = [(concatMap fst matches, matches)] -- concatenation into one big match
    lift roundup

-- | choice over a list of patterns
anyOf :: (Alternative f) => [f a] -> f a
anyOf = foldr (<|>) empty

-- | conditional rewriting: 'fail' when predicate is not met
require :: (MonadFail m) => Bool -> m ()
require p = unless p $ fail "requirement not met"

-- | 'fail' if given pattern succeeds, succeed if it fails.
requireFailure :: PatternT n a -> PatternT n ()
requireFailure p = require . not =<< probe p

-- | 'fail' when monadic predicate is not met
requireM :: (MonadFail m) => m Bool -> m ()
requireM p = p >>= require

-- some base patterns --------------------------------------------------------

-- | Lift a scrutiny from 'Reader' to 'Pattern' leaving the history unchanged.
liftReader :: Reader (Graph n) a -> PatternT n a
liftReader r = PatternT $ \h -> do
  x <- runReader r `liftM` ask
  return ([], x)

-- | any node anywhere in the graph
node :: (View v n) => PatternT n v
node = liftReader . inspectNode =<< branchNodes =<< liftReader readNodeList

-- | A specific node
nodeAt :: (View v n) => Node -> PatternT n v
nodeAt ref = do
  n <- liftReader $ inspectNode ref
  PatternT $ \h -> lift $ return ([ref], n)

-- | any edge anywhere in the graph
edge :: PatternT n Edge
edge = branch =<< liftReader readEdgeList

-- | node that is connected to given edge
nodeWith :: (View v n) => Edge -> PatternT n v
nodeWith e = liftReader . inspectNode =<< branchNodes =<< liftReader (attachedNodes e)

-- | edge that is attached to given node
edgeOf :: (View [Port] n) => Node -> PatternT n Edge
edgeOf n = branch =<< liftReader (attachedEdges n)

-- | node that is connected to the given node, but not that node itself
neighbour :: (View [Port] n, View v n) => Node -> PatternT n v
neighbour n = liftReader . inspectNode =<< branchNodes =<< liftReader (neighbours n)

-- | node that is connected to the given node, permitting the node itself
relative :: (View [Port] n, View v n) => Node -> PatternT n v
relative n = liftReader . inspectNode =<< branchNodes =<< liftReader (relatives n)

{- | nodes connected to given port of the specified node, not including the node itself.
Consider as an alternative 'linear' combined with 'nodeWith'.
-}
adverse :: (View [Port] n, View v n) => Port -> Node -> PatternT n v
adverse p n = liftReader . inspectNode =<< branchNodes =<< liftReader (adverseNodes n p)

-- controlling history and future --------------------------------------------

-- | A specific node
visit :: Node -> PatternT n ()
visit n = do
  exists <- liftReader $ existNode n
  if exists
    then PatternT $ \h -> lift $ return ([n], ())
    else fail $ "visit: node with ID " ⧺ show n ⧺ " does not exist"

-- | Do not remember any of the nodes matched by the supplied pattern
amnesia :: PatternT n a -> PatternT n a
amnesia p = PatternT $ \h -> do
  (h', x) <- patternT p h
  return ([], x)

-- | list of nodes matched until now with the most recent node in head position
history :: PatternT n Match
history = PatternT $ \h -> return ([], h)

-- | a reference to the lastly matched node
previous :: PatternT n Node
previous = head <$> history

-- | only match nodes in the next pattern that have not been matched before
nextFresh :: PatternT n a -> PatternT n a
nextFresh = restrictOverlap $ \past future -> null future ∨ not (head future ∈ past)

-- | only accept the given node in the next match
nextIs :: Node -> PatternT n a -> PatternT n a
nextIs next = restrictOverlap $ \past future -> not (null future) ∧ head future ≡ next

-- | Restrict a pattern based on the which of nodes have been matched previously and which nodes will be matched in the future. The first parameter of the supplied function is the history with the most recently matched node in head position. The second parameter is the future with the next matched node in head position.
restrictOverlap :: (Match -> Match -> Bool) -> PatternT n a -> PatternT n a
restrictOverlap c p = PatternT $ \h -> do
  (h', x) <- patternT p h
  require $ c h h'
  return (h', x)

-- TODO: the check is only done after the whole pattern has matched (maybe do the check more often inbetween?)

-- | Nodes in the future may not be matched more than once.
linear :: PatternT n a -> PatternT n a
linear = restrictOverlap $ \hist future -> isLinear Set.empty future
 where
  isLinear left [] = True
  isLinear left (r : rs) = not (r `Set.member` left) ∧ isLinear (r `Set.insert` left) rs
