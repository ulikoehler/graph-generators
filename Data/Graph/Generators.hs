{-# LANGUAGE Safe #-}

module Data.Graph.Generators where

{-|
    The information required to build a graph.

    This datastructure is designed to occupy minimal space.
    With n being the number of nodes, the edge list contains
    tuples (from, to), denoting an edge from node *from* to node
    *to* where *from* and *to* are integers less than the number
    of nodes.

    Note that for a graph with n nodes, the nodes are labelled
    @[0..n-1]@.

    This data structure is library-agnostic and can be converted
    to arbitrary representations.

    Copyright (C) 2014 Uli Köhler
    Apache License v2.0
-}
data GraphInfo = GraphInfo {
                  numNodes :: Int, -- ^ Number of nodes
                  edges :: [(Int,Int)] -- ^ Edge list
                 } deriving (Eq, Show)

{-|
    The context of a single graph node.

    This data-structure is library-agnostic, however,
    it is isomophic to FGL's UContext
-}
data GraphContext = GraphContext {
                        inEdges :: [Int], -- ^ Nodes having an edge to the current node
                        nodeLabel :: Int, -- ^ The node identifier of the current node
                        outEdges :: [Int] -- ^ Nodes having an ingoing edge from the current node
                    }

{-|
    Check the integrity of a GraphInfo instance:
    Ensures for every edge (i,j), the following condition is met:
    @0 <= i < n && 0 <= j < n@
-}
checkGraphInfo :: GraphInfo -> Bool
checkGraphInfo (GraphInfo n edges) =
    all (\(i, j) -> 0 <= i && i < n && 0 <= j && j < n) edges

-- | Get the edge count for a given GraphInfo instance
numEdges :: GraphInfo -> Int
numEdges = length . edges