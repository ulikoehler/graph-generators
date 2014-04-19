{-# LANGUAGE Safe #-}

{-
    Graph generators for simple parametric, regular graphs.

    Built using NetworkX 1.8.1, see <http://networkx.github.io/documentation/latest/reference/generators.html NetworkX Generators>
-}
module Data.Graph.Generators.Regular (
        completeGraph,
        completeGraphWithSelfloops,
        completeBipartiteGraph,
        emptyGraph,
        barbellGraph,
        generalizedBarbellGraph,
        cycleGraph,
        lineGraph
    ) where

import Data.Graph.Generators

{-
    Generate a completely connected graph with n nodes.

    The generated graph contains node labels [0..n-1]

    In contrast to 'completeGraphWithSelfloops' this function
    does not generate self-loops.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected. The generated edge (i,j) satisfied @i < j@.
-}
completeGraph :: Int -- ^ The number of nodes in the graph
              -> GraphInfo -- ^ The resulting complete graph
completeGraph n =
    let allNodes = [0..n-1]
        allEdges = [(i, j) | i <- allNodes,j <- allNodes, i < j]
    in GraphInfo n allEdges

{-
    Variant of 'completeGraph' generating self-loops.

    All generated edges (i,j) satisfy @i <= j@.

    See 'completeGraph' for a more detailed behaviour description
-}
completeGraphWithSelfloops :: Int -- ^ The number of nodes in the graph
                         -> GraphInfo -- ^ The resulting complete graph
completeGraphWithSelfloops n =
    let allNodes = [0..n-1]
        allEdges = [(i, j) | i <- allNodes, j <- allNodes, i <= j]
    in GraphInfo n allEdges

{-
    Generate the complete bipartite graph with n1 nodes in
    the first partition and n2 nodes in the second partition.

    Each node in the first partition is connected to each node
    in the second partition.

    The first partition nodes are identified by [0..n1-1]
    while the nodes in the second partition are identified
    by [n1..n1+n2-1]

    Use 'Data.Graph.Inductive.Basic.undir' to also add edges
    from the second partition to the first partition.
-}
completeBipartiteGraph :: Int -- ^ The number of nodes in the first partition
                       -> Int -- ^ The number of nodes in the second partition
                       -> GraphInfo -- ^ The resulting graph
completeBipartiteGraph n1 n2 =
    let nodesP1 = [0..n1-1]
        nodesP2 = [n1..n1+n2-1]
        allEdges = [(i, j) | i <- nodesP1, j <- nodesP2]
    in GraphInfo (n1+n2) allEdges

{-
    Generates the empty graph with n nodes and zero edges.

    The nodes are labelled [0..n-1]
-}
emptyGraph :: Int -> GraphInfo
emptyGraph n = GraphInfo n []

{-
    Generate the barbell graph, consisting of two complete subgraphs
    connected by a single path.

    In contrast to 'generalizedBarbellGraph', this function always
    generates identically-sized bells. Therefore this is a special
    case of 'generalizedBarbellGraph'
-}
barbellGraph :: Int -- ^ The number of nodes in the complete bells
             -> Int -- ^ The number of nodes in the path,
                    --   i.e the number of nodes outside the bells
             -> GraphInfo -- ^ The resulting barbell graph
barbellGraph n np = generalizedBarbellGraph n np n

{-
    Generate the barbell graph, consisting of two complete subgraphs
    connected by a single path.

    Self-loops are not generated.

    The nodes in the first bell are identified by [0..n1-1]
    The nodes in the path are identified by [n1..n1+np-1]
    The nodes in the second bell are identified by [n1+np..n1+np+n2-1]

    The path only contains edges 
-}
generalizedBarbellGraph :: Int -- ^ The number of nodes in the first bell
                        -> Int -- ^ The number of nodes in the path, i.e.
                               --   the number of nodes outside the bells
                        -> Int -- ^ The number of nodes in the second bell
                        -> GraphInfo -- ^ The resulting barbell graph
generalizedBarbellGraph n1 np n2 =
    let nodesP1 = [0..n1-1]
        nodesPath = [n1..n1+np-1]
        nodesP2 = [n1+np..n1+np+n2-1]
        edgesP1 = [(i, j) | i <- nodesP1, j <- nodesP1, i /= 2]
        edgesPath = [(i, i+1) | i <- [n1+np..n1+np+n2]]
        edgesP2 = [(i, j) | i <- nodesP2, j <- nodesP2]
    in GraphInfo (n1+np+n2) (edgesP1 ++ edgesPath ++ edgesP2)

{-
    Generate the cycle graph of size n.

    Edges are created from lower node IDs to higher node IDs.
-}
cycleGraph :: Int -- ^ n: Number of nodes in the circle
           -> GraphInfo -- ^ The circular graph with n nodes.
cycleGraph n =
    let edges = (n-1, 0) : [(i, i+1) | i <- [0..n-2]]
    in GraphInfo n edges

{-
    Generate the line graph of size n
-}
lineGraph :: Int -- ^ n: Number of nodes
          -> GraphInfo
lineGraph n =
    let edges = [(i, i+1) | i <- [0..n-2]]
    in GraphInfo n edges