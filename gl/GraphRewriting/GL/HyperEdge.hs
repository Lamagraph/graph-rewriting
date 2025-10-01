{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}

module GraphRewriting.GL.HyperEdge where

import GraphRewriting.Graph
import GraphRewriting.Graph.Read
import GraphRewriting.Layout.PortSpec
import GraphRewriting.Layout.Position
import GraphRewriting.Layout.RotPortSpec
import GraphRewriting.Layout.Rotation

type HyperEdgeRepr n = Edge -> [n] -> [(Vector2, Vector2)]

star :: (View [Port] n, View Position n, PortSpec n, View Rotation n) => HyperEdgeRepr n
star e ns = [(port, focalPoint ports) | port <- ports]
 where
  ports = concatMap (propOfPort absRotPortPos e) ns

complete :: (View Position n, PortSpec n, View Rotation n) => Edge -> [n] -> [(Vector2, Vector2)]
complete = undefined
