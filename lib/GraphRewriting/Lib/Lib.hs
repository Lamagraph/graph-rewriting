{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{- | This module provides an easy-to-use interface to create an interactive, graphical front-end for you graph rewriting system. The controls of the GUI are as follows:

- Left-click on a menu entry to /select/ a rewriting rule. At all times all redexes with respect to the selected rule are marked red in the graph. Note that the menu is hierarchical, which means that selecting a rule that has subordinate entries has the effect of all these entries being selected.

- Right-click on a menu entry to apply the corresponding rule at every applicable position in the graph simultaneously (in no particular order). Redexes that are destroyed (or created) by prior contractions in this process are not reduced, thus if single applications of the rule terminate, so does its simultaneous application. Right-clicking does /not/ select the rule.

- Right-click on a node of the graph to apply the selected rewriting rule at that position. You know before whether it is a applicable, since all redexes in the graph with respect to the selected rule are marked red. Right-clicking on a non-redex node has no effect. The layouting stops while the right mouse-button is pressed.

- Drag the background of the canvas to scroll around.

- Drag individual nodes of the graph around to manually change the layouting of the graph.

- Use your mouse-wheel to zoom in/out. Make sure to keep the mouse curser in the canvas area and not the menu while zooming.

- Press space to pause/resume layouting. Currently layouting is automatically resumed when the graph is rewritten by right-clicking on an individual node and not when right-clicking on a menu entry. This also requires the mouse cursor to be positioned in the canvas area.

Please have a look the graph-rewriting-ski package for an example application that makes use of this library.
-}
module GraphRewriting.Lib.Lib where

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
