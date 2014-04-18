module Data.Graph.Generators where

{-
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
-}
data GraphInfo = GraphInfo {
                  numNodes :: Int, -- ^ Number of nodes
                  edges :: [(Int,Int)] -- ^ Edge list
                 }

{-
    The context of a single graph node.

    This data-structure is library-agnostic, however,
    it is isomophic to FGL's UContext
-}
data GraphContext = GraphContext {
                        inEdges :: [Int], -- ^ Nodes having an edge to the current node
                        nodeLabel :: Int, -- ^ The node identifier of the current node
                        outEdges :: [Int] -- ^ Nodes having an ingoing edge from the current node
                    }