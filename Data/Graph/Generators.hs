{-
  Generators for deterministic graphs
-}

module Data.Graph.Generators (
    ) where

import Data.Graph.Inductive

{-
    Generate a completely connected graph with n nodes.

    The generated graph contains node labels [1..n]

    In contrast to 'completeGraphWithSelfloops' this function
    does not generate self-loops.

    Note that the resulting graph contains both edge directions.
-}
completeGraph :: Int -- ^ The number of nodes in the graph
              -> UGr -- ^ The resulting complete graph
completeGraph n =
    let allNodes = [1..n]
        allEdges = [(i, j) | i <- allNodes, j <- allNodes, i /= j]
    in mkUGraph allNodes allEdges

{-
    Variant of 'completeGraph' generating self-loops.

    See 'completeGraph' for a more detailed behaviour description
-}
completeGraphWithSelfloops :: Int -- ^ The number of nodes in the graph
                         -> UGr -- ^ The resulting complete graph
completeGraphWithSelfloops n =
    let allNodes = [1..n]
        allEdges = [(i, j) | i <- allNodes, j <- allNodes]
    in mkUGraph allNodes allEdges

{-
    Generate the complete bipartite graph with n1 nodes in
    the first partition and n2 nodes in the second partition.

    Each node in the first partition is connected to each node
    in the second partition.

    The first partition nodes are identified by [1..n1]
    while the nodes in the second partition are identified
    by [n1+1..n1+n2]
-}
completeBipartiteGraph :: Int -- ^ The number of nodes in the first partition
                       -> Int -- ^ The number of nodes in the second partition
                       -> UGr -- ^ The resulting graph
completeBipartiteGraph n1 n2 =
    let nodesP1 = [1..n1]
        nodesP2 = [n1+1..n1+n2]
        allEdges = [(i, j) | i <- nodesP1, j <- nodesP2]
    in mkUGraph (nodesP1 ++ nodesP2) allEdges

{-
    Generates the empty graph with n nodes and zero edges.

    The nodes are labelled [1..n]
-}
emptyGraph :: Int -> UGr
emptyGraph n = mkUGraph [1..n] []

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
             -> UGr -- ^ The resulting barbell graph
barbellGraph n np = generalizedBarbellGraph n np n

{-
    Generate the barbell graph, consisting of two complete subgraphs
    connected by a single path.

    Self-loops are not generated.

    The nodes in the first bell are identified by [1..n1]
    The nodes in the path are identified by [n1+1..n1+np]
    The nodes in the second bell are identified by [n1+np+1..n1+np+n2]

    The path only contains edges 
-}
generalizedBarbellGraph :: Int -- ^ The number of nodes in the first bell
                        -> Int -- ^ The number of nodes in the path, i.e.
                               --   the number of nodes outside the bells
                        -> Int -- ^ The number of nodes in the second bell
                        -> UGr -- ^ The resulting barbell graph
generalizedBarbellGraph n1 np n2 =
    let nodesP1 = [1..n1]
        nodesPath = [n1+1..n1+np]
        nodesP2 = [n1+np+1..n1+np+n2]
        edgesP1 = [(i, j) | i <- nodesP1, j <- nodesP1, i /= 2]
        edgesPath = [(i, i+1) | i <- [n1+np..n1+np+n2]]
        edgesP2 = [(i, j) | i <- nodesP2, j <- nodesP2]
    in mkUGraph (nodesP1 ++ nodesPath ++ nodesP2) (edgesP1 ++ edgesPath ++ edgesP2)

{-
    Generates the Bull graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected

@
    1       2
     \     /
      3---4
       \ /
        5
@
-}
bullGraph :: UGr
bullGraph =
    let nodes = [1..5]
        edges = [(1,3),(2,4),(3,4),(3,5),(4,5)]
    in mkUGraph nodes edges

{-
    Generate the Frucht Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected

    See <http://mathworld.wolfram.com/FruchtGraph.html >
-}
fruchtGraph :: UGr
fruchtGraph =
    let nodes = [1..12]
        edges = [(1,8),(2,8),(3,9),(4,10),(5,10),(6,11),(7,11),
                (8,12),(9,12),(9,10),(11,12)]
    in mkUGraph nodes edges

{-
    Generate the house graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected

@
    1
   / \
  2---3
  |   |
  4---5
@

-}
houseGraph :: UGr
houseGraph =
    let nodes = [1..5]
        edges = [(1,2),(1,3),(2,3),(2,4),(3,5),(3,5)]
    in mkUGraph nodes edges

{-
    Generate the house X graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected

@
    1
   / \
  2---3
  | X |
  4---5
@

-}
houseXGraph :: UGr
houseXGraph =
    let nodes = [1..5]
        edges = [(1,2),(1,3),(2,3),(2,4),(3,5),(3,5),(2,5),(3,4)]
    in mkUGraph nodes edges