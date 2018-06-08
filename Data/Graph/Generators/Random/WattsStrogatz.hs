{-|
  Implementations of binomially random graphs, as described by Erdős and Rényi.

  Graphs generated using this method have a constant edge probability between two nodes.

  See Erdős and A. Rényi, On Random Graphs, Publ. Math. 6, 290 (1959).

  graph-generators copyright:
    Copyright (C) 2014 Uli Köhler

  NetworkX copyright:
    Copyright (C) 2004-2010 by 
    Aric Hagberg <hagberg@lanl.gov>
    Dan Schult <dschult@colgate.edu>
    Pieter Swart <swart@lanl.gov>
    All rights reserved.
    BSD license.
-}
module Data.Graph.Generators.Random.WattsStrogatz (
  -- ** Graph generators
        wattsStrogatzGraph,
        wattsStrogatzGraph',
        -- ** Graph component generators
        wattsStrogatzContext,
        -- ** Utility functions
        selectWithProbability
    )
    where

import System.Random.MWC
import Control.Monad
import Data.Graph.Generators
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import qualified Data.Set as Set

{-|
    Generate a small-world context using the Wattz Strogatz method.

    See 'wattsStrogatzGraph' for a detailed algorithm description.

    Example usage, using a truly random generator:
    
    > import System.Random.MWC
    > gen <- withSystemRandom . asGenIO $ return
    > 
-}
wattsStrogatzContext :: GenIO  -- ^ The random number generator to use
           -> Int     -- ^ Identifier of the context's central node
           -> [Int]   -- ^ The algorithm will generate random edges to those nodes
                      --   from or to the given node
           -> Double  -- ^ The probability for any pair of nodes to be connected
           -> IO GraphContext -- ^ The resulting graph (IO required for randomness)
wattsStrogatzContext gen n allNodes p = do
    let endpoints = selectWithProbability gen p allNodes
    inEdges <- endpoints
    outEdges <- endpoints
    return $ GraphContext inEdges n outEdges

{-|
    Generate a unlabelled undirected random graph using the Algorithm introduced by
    WattsStrogatz.

    Note that self-loops with also be generated with probability p.

    This algorithm runs in O(kn).

    The generated nodes are identified by [0..n-1].

    Example usage, using a truly random generator:
    
    > import System.Random.MWC
    > gen <- withSystemRandom . asGenIO $ return
    > wattsStrogatzGraph gen 1000 10 0.6
    ...
    
-}
wattsStrogatzGraph :: GenIO  -- ^ The random number generator to use
           -> Int    -- ^ n, The number of nodes
           -> Int    -- ^ k, the size of the neighborhood / degree (should be even)
           -> Double -- ^ \beta, The probability of a forward edge getting rewritten
           -> IO GraphInfo -- ^ The resulting graph (IO required for randomness)
wattsStrogatzGraph gen n k p = do
  let allNodes = [0..n-1]
  -- Outgoing edge targets for any node
  let insert m (i, js) = Map.insert i js m
  let initialEdges = foldl (insert) (Map.empty) [ (i, forward_neighbors i) | i <- allNodes ]
  allEdges <- rewrites (return initialEdges) $
              [ (i, j) |
                (i,s) <- Map.toList initialEdges,
                j <- Set.toList s ]
  return $ GraphInfo n (delineate allEdges)
  where
    k' = fromInteger.toInteger $ k
    forward_neighbors :: Int -> Set.Set Int 
    forward_neighbors i = foldr (Set.insert) Set.empty $
                          fmap (`mod` n)
                          [i+1..i+k']
    rewrites :: IO (Map.Map Int (Set.Set Int))
             -> [(Int, Int)]
             -> IO (Map.Map Int (Set.Set Int))
    rewrites ioedges [] = ioedges
    rewrites ioedges (t:tuples) = do
      r <- uniform gen :: IO Double
      edges <- ioedges :: IO (Map.Map Int (Set.Set Int))
      if (r > p)
        then rewrites (return edges) tuples
        else do es <- rewrite t edges
                rewrites (return es) tuples
    rewrite (i, j1) edges = do
      r <- uniform gen :: IO Double
      let j2 = floor $ r*((fromInteger.toInteger) n)
      if (((member (i, j2) edges) || (member (j2, i) edges)) || (i == j2)) 
        then rewrite (i, j1) edges
        else return $ swap (i, j1) (i, j2) edges

delineate :: Map.Map a (Set.Set b) -> [(a, b)]
delineate m = [ (i, j) |
                (i, js) <- Map.toList m,
                j <- Set.toList js ]

member :: (Int, Int) -> Map.Map Int (Set.Set Int) -> Bool
member (i, j) m = Set.member j (Map.findWithDefault Set.empty i m)

swap :: (Ord a, Ord b)
        => (a, b) -> (a, b)
        -> Map.Map a (Set.Set b)
        -> Map.Map a (Set.Set b)
swap (k1, v1) (k2, v2) m =
  let set1 = Map.findWithDefault Set.empty k1 m in
  let set2 = Set.insert v2 $ Set.delete v1 set1 in
  let m2 = Map.insert k2 set2 $ Map.delete k1 m in m2

{-|
    Like 'wattsStrogatzGraph', but uses a newly initialized random number generator.

    See 'System.Random.MWC.withSystemRandom' for details on how the generator is
    initialized.

    By using this function, you don't have to initialize the generator by yourself,
    however generator initialization is slow, so reusing the generator is recommended.

    Usage example:

    > wattsStrogatzGraph' 1000 10 0.6
-}
wattsStrogatzGraph' :: Int    -- ^ n, The number of nodes
                 -> Int    -- ^ k, the size of the neighborhood / degree (should be even)
                 -> Double -- ^ \beta, The probability of a forward edge getting rewritten
                 -> IO GraphInfo -- ^ The resulting graph (IO required for randomness)
wattsStrogatzGraph' n k p =
    withSystemRandom . asGenIO $ \gen -> wattsStrogatzGraph gen n k p

selectWithProbability :: GenIO  -- ^ The random generator state
                      -> Double -- ^ The probability to select each list element
                      -> [a]    -- ^ The list to filter
                      -> IO [a] -- ^ The filtered list  
selectWithProbability _   _ [] = return []
selectWithProbability gen p (x:xs) = do
    r <- uniform gen :: IO Double
    let v = [ x | r <= p ]
    liftM2 (++) (return v) $ selectWithProbability gen p xs
