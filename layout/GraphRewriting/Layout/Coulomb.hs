{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}

module GraphRewriting.Layout.Coulomb where

import Data.Functor ()
import Data.View
import GraphRewriting.Graph.Read
import GraphRewriting.Graph.Types
import GraphRewriting.Layout.Force
import GraphRewriting.Layout.Position
import GraphRewriting.Pattern ()

coulombForce :: (View Position n) => Node -> WithGraph n Force
coulombForce node = do
  n <- examine position <$> readNode node
  ns <- fmap (map $ examine position) (mapM readNode =<< readNodeList)
  return $ fsum [repulsion n' n | n' <- ns]
