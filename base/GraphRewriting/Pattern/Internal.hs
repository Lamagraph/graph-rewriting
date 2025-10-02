{-# LANGUAGE UnicodeSyntax #-}

module GraphRewriting.Pattern.Internal where

import Control.Monad.Reader
import GraphRewriting.Graph.Types

-- TODO: change the dependency of Match into a ReaderT Match?

-- | A pattern represents a graph scrutiny that memorises all the scrutinised nodes during matching.
newtype PatternT n a = PatternT {patternT :: Match -> ReaderT (Graph n) [] (Match, a)}

runPatternT' :: Match -> PatternT n a -> Graph n -> [(Match, a)]
runPatternT' h p = runReaderT (patternT p h)

-- | Nodes matched in the evaluation of a pattern with the lastly matched node at the head
type Match = [Node]
