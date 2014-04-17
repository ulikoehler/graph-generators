{-
  Generators for deterministic graphs
-}

module Data.Graph.Generators (
    ) where

import Data.Graph.Inductive

{-
    Generate a completely connected graph with n nodes.

    The generated graph contains node labels [0..n-1]

    In contrast to 'completeGraphWithSelfloops' this function
    does not generate self-loops.

    Note that the resulting graph contains both edge directions.
-}
completeGraph :: Int -- ^ The number of nodes in the graph
              -> UGr -- ^ The resulting complete graph
completeGraph n =
    let allNodes = [0..n-1]
        allEdges = [(i,j) | i <- allNodes, j <- allNodes, i /= j]
    in mkUGraph allNodes allEdges

{-
    Variant of 'completeGraph' generating self-loops.

    See 'completeGraph' for a more detailed behaviour description
-}
completeGraphWithSelfloops :: Int -- ^ The number of nodes in the graph
                         -> UGr -- ^ The resulting complete graph
completeGraphWithSelfloops n =
    let allNodes = [0..n-1]
        allEdges = [(i, j) | i <- allNodes, j <- allNodes]
    in mkUGraph allNodes allEdges

{-
    Generate the complete bipartite graph with n1 nodes in
    the first partition and n2 nodes in the second partition.

    Each node in the first partition is connected to each node
    in the second partition.

    The first partition nodes are identified by [0..n1-1]
    while the nodes in the second partition are identified
    by [n1..n1+n2-1]
-}
completeBipartiteGraph :: Int -- ^ The number of nodes in the first partition
                       -> Int -- ^ The number of nodes in the second partition
                       -> UGr -- ^ The resulting graph
completeBipartiteGraph n1 n2 =
    let nodesP1 = [0..n1-1]
        nodesP2 = [n1..n1+n2-1]
        allEdges = [(i, j) | i <- nodesP1, j <- nodesP2]
    in mkUGraph (nodesP1 ++ nodesP2) allEdges

{-
    Generates the empty graph with n nodes and zero edges.

    The nodes are labelled [0..n-1]
-}
emptyGraph :: Int -> UGr
emptyGraph n = mkUGraph [0..n-1] []

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

    The nodes in the first bell are identified by [0..n1-1]
    The nodes in the path are identified by [n1..n1+np-1]
    The nodes in the second bell are identified by [n1+np..n1+np+n2-1]

    The path only contains edges 
-}
generalizedBarbellGraph :: Int -- ^ The number of nodes in the first bell
                        -> Int -- ^ The number of nodes in the path, i.e.
                               --   the number of nodes outside the bells
                        -> Int -- ^ The number of nodes in the second bell
                        -> UGr -- ^ The resulting barbell graph
generalizedBarbellGraph n1 np n2 =
    let nodesP1 = [0..n1-1]
        nodesPath = [n1..n1+np-1]
        nodesP2 = [n1+np..n1+np+n2-1]
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
    0       1
     \     /
      2---3
       \ /
        4
@
-}
bullGraph :: UGr
bullGraph =
    let nodes = [0..4]
        edges = [(0,2),(1,3),(2,3),(2,4),(3,4)]
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
    let nodes = [0..11]
        edges = [(0,1),(0,6),(0,7),(1,2),(1,7),(2,8),(2,3),
                 (3,9),(3,4),(4,9),(4,5),(5,10),(5,6),
                 (6,10),(7,11),(8,9),(8,11),(10,11)]
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
    let nodes = [0..4]
        edges = [(0,1),(0,2),(1,3),(2,3),(2,4),(3,4)]
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
    let nodes = [0..4]
        edges = [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3),(2,4),(3,4)]
    in mkUGraph nodes edges

{-
    Generate the Pappus Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.

    Nodes are labelled [0..17]
-}
pappusGraph :: UGr
pappusGraph =
    let nodes = [0..17]
        edges = [(0,1),(0,5),(0,17),(1,8),(1,2),(2,3),(2,13),(3,4),
                (3,10),(4,5),(4,15),(5,6),(6,11),(6,7),(7,8),(7,14),
                (8,9),(9,16),(9,10),(10,11),(11,12),(12,17),(12,13),
                (13,14),(14,15),(15,16),(16,17)]
    in mkUGraph nodes edges

{-
    Generate the Sedgewick Maze Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
sedgewickMazeGraph :: UGr
sedgewickMazeGraph =
    let nodes = [0..7]
        edges = [(0,2),(0,5),(0,7),(1,7),(2,6),
                (3,4),(3,5),(4,5),(4,6),(4,7)]
    in mkUGraph nodes edges

{-
    Generate the Petersen Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
petersenGraph :: UGr
petersenGraph =
    let nodes = [0..9]
        edges = [(0,1),(0,4),(0,5),(1,2),(1,6),(2,3),
                 (2,7),(3,8),(3,4),(4,9),(5,8),(5,7),
                 (6,8),(6,9),(7,9)]
    in mkUGraph nodes edges

{-
    Generate the Heawood Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
heawoodGraph :: UGr
heawoodGraph =
    let nodes = [0..13]
        edges = [(0,1),(0,13),(0,5),(1,2),(1,10),(2,3),
                 (2,7),(3,12),(3,4),(4,9),(4,5),(5,6),
                 (6,11),(6,7),(7,8),(8,9),(8,13),(9,10),
                 (10,11),(11,12),(12,13)]
    in mkUGraph nodes edges

{-
    Generate the Diamond Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
diamondGraph :: UGr
diamondGraph =
    let nodes = [0..3]
        edges = [(0,1),(0,2),(1,2),(1,3),(2,3)]
    in mkUGraph nodes edges

{-
    Generate the dodecahedral Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
dodecahedralGraph :: UGr
dodecahedralGraph =
    let nodes = [0..19]
        edges = [(0,1),(0,10),(0,19),(1,8),(1,2),(2,3),
                 (2,6),(3,19),(3,4),(4,17),(4,5),(5,6),
                 (5,15),(6,7),(7,8),(7,14),(8,9),(9,10),
                 (9,13),(10,11),(11,12),(11,18),(12,16),
                 (12,13),(13,14),(14,15),(15,16),(16,17),
                 (17,18),(18,19)]
    in mkUGraph nodes edges

{-
    Generate the icosahedral Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
icosahedralGraph :: UGr
icosahedralGraph =
    let nodes = [0..11]
        edges = [(0,8),(0,1),(0,11),(0,5),(0,7),(1,8),(1,2),
                 (1,5),(1,6),(2,8),(2,3),(2,6),(2,9),(3,9),
                 (3,4),(3,10),(3,6),(4,11),(4,10),(4,5),(4,6),
                 (5,11),(5,6),(7,8),(7,10),(7,11),(7,9),(8,9),
                 (9,10),(10,11)]
    in mkUGraph nodes edges

{-
    Generate the Krackhardt Kite Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
krackhardtKiteGraph :: UGr
krackhardtKiteGraph =
    let nodes = [0..9]
        edges = [(0,8),(0,1),(0,11),(0,5),(0,7),(1,8),(1,2),(1,5),
                 (1,6),(2,8),(2,3),(2,6),(2,9),(3,9),(3,4),(3,10),
                 (3,6),(4,11),(4,10),(4,5),(4,6),(5,11),(5,6),(7,8),
                 (7,10),(7,11),(7,9),(8,9),(9,10),(10,11)]
    in mkUGraph nodes edges

moebius_kantor_graph :: UGr
