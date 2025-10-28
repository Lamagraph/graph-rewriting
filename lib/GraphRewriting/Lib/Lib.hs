{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module GraphRewriting.Lib.Lib (run, LabelledTree (..), showLabelledTree) where

import Control.Monad
import Data.IORef
import Data.Set as Set
import GraphRewriting.Graph
import GraphRewriting.Graph.Read
import GraphRewriting.Lib.Global
import GraphRewriting.Rule

run ::
  (Show n, Show n', Eq n, Eq n') =>
  -- | The number of initial layout steps to apply before displaying the graph
  Int ->
  -- | A projection function that is applied just before displaying the graph (KubEF: id function :D )
  (Graph n -> Graph n') ->
  -- | The monadic graph transformation code for a layout step
  (Node -> Rewrite n a) ->
  -- | (KubEF: execution graph)
  Graph n ->
  -- | The rule menu given as a tree of named rules (KubEF: map: @name rule@ -> @rule@)
  LabelledTree (Rule n) ->
  IO (Graph n)
run initSteps project layoutStep g rules = do
  globalVars <-
    newIORef $
      GlobalVars
        { graph = execGraph (replicateM_ initSteps $ mapM layoutStep =<< readNodeList) g
        , selectedRule = 0
        , layoutStep = void . layoutStep
        , getRules = fmap (0,) rules
        }
  computeGraph globalVars
  graph <$> readIORef globalVars

computeGraph :: (Show n, Eq n) => IORef (GlobalVars n) -> IO ()
computeGraph globalVars = do
  gv <- readIORef globalVars
  takeAStep globalVars
  -- Just for debug properties
  print . graph <$> readIORef globalVars
  actives <- highlight globalVars
  if Set.null actives then return () else computeGraph globalVars

takeAStep :: (Show n) => IORef (GlobalVars n) -> IO ()
takeAStep globalVars = do
  gv <- readIORef globalVars
  applyLeafRules id 0 globalVars
  return ()
