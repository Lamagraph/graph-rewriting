{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}

module GraphRewriting.Layout.Gravitation where

import Data.Functor ()
import Data.Vector.Class
import Data.Vector.V2 ()
import Data.View
import GraphRewriting.Graph.Read
import GraphRewriting.Graph.Types
import GraphRewriting.Layout.Force
import GraphRewriting.Layout.Position
import GraphRewriting.Pattern ()

centralGravitation :: (View Position n) => Node -> WithGraph n Force
centralGravitation = fmap (attraction (vpromote 0) . examine position) . readNode

gravitation :: (View Position n) => Node -> Rewrite n Force
gravitation node = do
  n <- examine position <$> readNode node
  ns <- fmap (map $ examine position) (mapM readNode =<< readNodeList)
  return $ fsum [attraction n' n | n' <- ns]
