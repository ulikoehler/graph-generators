{-# LANGUAGE Safe #-}

{-|
  Generators for classic non-parametric graphs.

  Built using NetworkX 1.8.1, see <http://networkx.github.io/documentation/latest/reference/generators.html NetworkX Generators>

  graph-generators copyright:
    Copyright (C) 2014 Uli Köhler
    Apache License v2.0

  NetworkX copyright:
    Copyright (C) 2004-2010 by 
    Aric Hagberg <hagberg@lanl.gov>
    Dan Schult <dschult@colgate.edu>
    Pieter Swart <swart@lanl.gov>
    All rights reserved.
    BSD license.
-}

module Data.Graph.Generators.Classic (
        trivialGraph,
        bullGraph,
        chvatalGraph,
        cubicalGraph,
        desarguesGraph,
        diamondGraph,
        dodecahedralGraph,
        fruchtGraph,
        heawoodGraph,
        houseGraph,
        houseXGraph,
        icosahedralGraph,
        krackhardtKiteGraph,
        moebiusKantorGraph,
        octahedralGraph,
        pappusGraph,
        petersenGraph,
        sedgewickMazeGraph,
        tetrahedralGraph,
        truncatedCubeGraph,
        truncatedTetrahedronGraph,
        tutteGraph,
        nullGraph
    ) where

import Data.Graph.Generators

{-|
    Generates the trivial graph, containing only one node
    and no edges
-}
trivialGraph :: GraphInfo
trivialGraph = GraphInfo 1 []

{-|
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
bullGraph :: GraphInfo
bullGraph =
    let edges = [(0,2),(1,3),(2,3),(2,4),(3,4)]
    in GraphInfo 5 edges

{-|
    Generate the Frucht Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected

    See <http://mathworld.wolfram.com/FruchtGraph.html >
-}
fruchtGraph :: GraphInfo
fruchtGraph =
    let edges = [(0,1),(0,6),(0,7),(1,2),(1,7),(2,8),(2,3),
                 (3,9),(3,4),(4,9),(4,5),(5,10),(5,6),
                 (6,10),(7,11),(8,9),(8,11),(10,11)]
    in GraphInfo 12 edges

{-|
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
houseGraph :: GraphInfo
houseGraph =
    let edges = [(0,1),(0,2),(1,3),(2,3),(2,4),(3,4)]
    in GraphInfo 5 edges

{-|
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
houseXGraph :: GraphInfo
houseXGraph =
    let edges = [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3),(2,4),(3,4)]
    in GraphInfo 5 edges

{-|
    Generate the Pappus Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.

    Nodes are labelled [0..17]
-}
pappusGraph :: GraphInfo
pappusGraph =
    let edges = [(0,1),(0,5),(0,17),(1,8),(1,2),(2,3),(2,13),(3,4),
                (3,10),(4,5),(4,15),(5,6),(6,11),(6,7),(7,8),(7,14),
                (8,9),(9,16),(9,10),(10,11),(11,12),(12,17),(12,13),
                (13,14),(14,15),(15,16),(16,17)]
    in GraphInfo 18 edges

{-|
    Generate the Sedgewick Maze Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
sedgewickMazeGraph :: GraphInfo
sedgewickMazeGraph =
    let edges = [(0,2),(0,5),(0,7),(1,7),(2,6),
                (3,4),(3,5),(4,5),(4,6),(4,7)]
    in GraphInfo 8 edges

{-|
    Generate the Petersen Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
petersenGraph :: GraphInfo
petersenGraph =
    let edges = [(0,1),(0,4),(0,5),(1,2),(1,6),(2,3),
                 (2,7),(3,8),(3,4),(4,9),(5,8),(5,7),
                 (6,8),(6,9),(7,9)]
    in GraphInfo 10 edges

{-|
    Generate the Heawood Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
heawoodGraph :: GraphInfo
heawoodGraph =
    let edges = [(0,1),(0,13),(0,5),(1,2),(1,10),(2,3),
                 (2,7),(3,12),(3,4),(4,9),(4,5),(5,6),
                 (6,11),(6,7),(7,8),(8,9),(8,13),(9,10),
                 (10,11),(11,12),(12,13)]
    in GraphInfo 14 edges

{-|
    Generate the Diamond Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
diamondGraph :: GraphInfo
diamondGraph =
    let edges = [(0,1),(0,2),(1,2),(1,3),(2,3)]
    in GraphInfo 4 edges

{-|
    Generate the dodecahedral Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
dodecahedralGraph :: GraphInfo
dodecahedralGraph =
    let edges = [(0,1),(0,10),(0,19),(1,8),(1,2),(2,3),
                 (2,6),(3,19),(3,4),(4,17),(4,5),(5,6),
                 (5,15),(6,7),(7,8),(7,14),(8,9),(9,10),
                 (9,13),(10,11),(11,12),(11,18),(12,16),
                 (12,13),(13,14),(14,15),(15,16),(16,17),
                 (17,18),(18,19)]
    in GraphInfo 20 edges

{-|
    Generate the icosahedral Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
icosahedralGraph :: GraphInfo
icosahedralGraph =
    let edges = [(0,8),(0,1),(0,11),(0,5),(0,7),(1,8),(1,2),
                 (1,5),(1,6),(2,8),(2,3),(2,6),(2,9),(3,9),
                 (3,4),(3,10),(3,6),(4,11),(4,10),(4,5),(4,6),
                 (5,11),(5,6),(7,8),(7,10),(7,11),(7,9),(8,9),
                 (9,10),(10,11)]
    in GraphInfo 12 edges

{-|
    Generate the Krackhardt-Kite Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
krackhardtKiteGraph :: GraphInfo
krackhardtKiteGraph =
    let edges = [(0,1),(0,2),(0,3),(0,5),(1,3),(1,4),(1,6),(2,3),
                 (2,5),(3,4),(3,5),(3,6),(4,6),(5,6),(5,7),(6,7),
                 (7,8),(8,9)]
    in GraphInfo 10 edges

{-|
    Generate the Möbius-Kantor Graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
moebiusKantorGraph :: GraphInfo
moebiusKantorGraph =
    let edges = [(0,1),(0,5),(0,15),(1,2),(1,12),(2,3),(2,7),(3,4),
                 (3,14),(4,9),(4,5),(5,6),(6,11),(6,7),(7,8),(8,9),
                 (8,13),(9,10),(10,11),(10,15),(11,12),(12,13),(13,14),(14,15)]
    in GraphInfo 16 edges


{-|
    Generate the octahedral graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
octahedralGraph :: GraphInfo
octahedralGraph =
    let edges = [(0,1),(0,2),(0,3),(0,4),(1,2),(1,3),(1,5),(2,4),
                 (2,5),(3,4),(3,5),(4,5)]
    in GraphInfo 6 edges

{-|
    Generate the Chvatal graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
chvatalGraph :: GraphInfo
chvatalGraph =
    let edges = [(0,1),(0,4),(0,6),(0,9),(1,2),(1,5),(1,7),(2,8),(2,3),
                 (2,6),(3,9),(3,4),(3,7),(4,8),(4,5),(5,10),(5,11),(6,11),
                 (6,10),(7,8),(7,11),(8,10),(9,11),(9,10)]
    in GraphInfo 12 edges

{-|
    Generate the cubical graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
cubicalGraph :: GraphInfo
cubicalGraph =
    let edges = [(0,1),(0,3),(0,4),(1,2),(1,7),(2,3),(2,6),(3,5),(4,5),
                 (4,7),(5,6),(6,7)]
    in GraphInfo 8 edges

{-|
    Generate the Desargues graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
desarguesGraph :: GraphInfo
desarguesGraph =
    let edges = [(0,1),(0,19),(0,5),(1,16),(1,2),(2,11),(2,3),(3,4),
                 (3,14),(4,9),(4,5),(5,6),(6,15),(6,7),(7,8),(7,18),
                 (8,9),(8,13),(9,10),(10,19),(10,11),(11,12),(12,17),
                 (12,13),(13,14),(14,15),(15,16),(16,17),(17,18),(18,19)]
    in GraphInfo 20 edges

{-|
    Generate the tetrahedral graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
tetrahedralGraph :: GraphInfo
tetrahedralGraph =
    let edges = [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)]
    in GraphInfo 4 edges

{-|
    Generate the truncated cube graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
truncatedCubeGraph :: GraphInfo
truncatedCubeGraph =
    let edges = [(0,1),(0,2),(0,4),(1,11),(1,14),(2,3),(2,4),(3,8),(3,6),
                 (4,5),(5,16),(5,18),(6,8),(6,7),(7,10),(7,12),(8,9),(9,17),
                 (9,20),(10,11),(10,12),(11,14),(12,13),(13,21),(13,22),
                 (14,15),(15,19),(15,23),(16,17),(16,18),(17,20),(18,19),
                 (19,23),(20,21),(21,22),(22,23)]
    in GraphInfo 24 edges

{-|
    Generate the truncated tetrahedron graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
truncatedTetrahedronGraph :: GraphInfo
truncatedTetrahedronGraph =
    let edges = [(0,1),(0,2),(0,9),(1,2),(1,6),(2,3),(3,11),(3,4),(4,11),
                 (4,5),(5,6),(5,7),(6,7),(7,8),(8,9),(8,10),(9,10),(10,11)]
    in GraphInfo 12 edges

{-|
    Generate the Tutte graph.

    Contains only one edge between two connected nodes,
    use 'Data.Graph.Inductive.Basic.undir' to make it
    quasi-undirected.
-}
tutteGraph :: GraphInfo
tutteGraph =
    let edges = [(0,1),(0,2),(0,3),(1,26),(1,4),(2,10),(2,11),(3,18),(3,19),
                 (4,5),(4,33),(5,29),(5,6),(6,27),(6,7),(7,8),(7,14),(8,9),
                 (8,38),(9,10),(9,37),(10,39),(11,12),(11,39),(12,35),(12,13),
                 (13,14),(13,15),(14,34),(15,16),(15,22),(16,17),(16,44),
                 (17,18),(17,43),(18,45),(19,20),(19,45),(20,41),(20,21),
                 (21,22),(21,23),(22,40),(23,24),(23,27),(24,32),(24,25),
                 (25,26),(25,31),(26,33),(27,28),(28,32),(28,29),(29,30),
                 (30,33),(30,31),(31,32),(34,35),(34,38),(35,36),(36,37),
                 (36,39),(37,38),(40,41),(40,44),(41,42),(42,43),(42,45),(43,44)]
    in GraphInfo 46 edges

{-|
    The null graph with no nodes and edges
-}
nullGraph :: GraphInfo
nullGraph = GraphInfo 0 []