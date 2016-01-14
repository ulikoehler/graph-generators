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
  let forward_neighbors i = foldl (Set.insert) Set.empty $ fmap (`mod` n) [i+1..i+(k/2)]
  let initialEdges = foldl (insert) (Map.empty)
      [ (i, forward_neighbors i) | i <- allNodes ]
  allEdges <- rewrites initialEdges $
              [ (i, j) | (i,s) <- Map.toList initialEdges, j <- toList s ]
  return $ GraphInfo n allEdges
  where
    rewrites edges [] = return edges
    rewrites edges (t:tuples) = do
      r <- uniform gen :: IO Double
      if (r > p)
        then return rewrites edges tuples
        else do es <- rewrite t edges
                return rewrites es tuples
    rewrite (i, j1) edges = do
      r <- uniform gen :: IO Double
      let j2 = Math.floor $ r*n
      if (edgeExists i j2 || edgeExists j2 i)
        then rewrite (i, j1) edges
        else return $ swap (i, j1) (i, j2) edges

member (i, j) = Set.member j (Map.findWithDefault Set.empty i edges))

swap :: (a, b) -> (a, b) -> Map a (Set b) -> Map a (Set b)
swap (k1, v1) (k2, v2) m =
  let set1 = Map.lookup k1 m in
  let set2 = Set.insert v2 $ Set.delete v1 set1 in
  let m2 = Map.insert k2 v2 $ Map.delete k1 m in m2

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
wattsStrogatzGraph' n p =
    withSystemRandom . asGenIO $ \gen -> wattsStrogatzGraph gen n p

selectWithProbability :: GenIO  -- ^ The random generator state
                      -> Double -- ^ The probability to select each list element
                      -> [a]    -- ^ The list to filter
                      -> IO [a] -- ^ The filtered list  
selectWithProbability _   _ [] = return []
selectWithProbability gen p (x:xs) = do
    r <- uniform gen :: IO Double
    let v = [ x | r <= p ]
    liftM2 (++) (return v) $ selectWithProbability gen p xs
