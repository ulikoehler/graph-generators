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
module Data.Graph.Generators.Random.ErdosRenyi (
        -- ** Graph generators
        erdosRenyiGraph,
        erdosRenyiGraph',
        -- ** Graph component generators
        erdosRenyiContext,
        -- ** Utility functions
        selectWithProbability
    )
    where

import System.Random.MWC
import Control.Monad
import Data.Graph.Generators
import Control.Applicative ((<$>))

{-|
    Generate a unlabelled context using the Erdős and Rényi method.

    See 'erdosRenyiGraph' for a detailed algorithm description.

    Example usage, using a truly random generator:
    
    > import System.Random.MWC
    > gen <- withSystemRandom . asGenIO $ return
    > 
-}
erdosRenyiContext :: GenIO  -- ^ The random number generator to use
           -> Int     -- ^ Identifier of the context's central node
           -> [Int]   -- ^ The algorithm will generate random edges to those nodes
                      --   from or to the given node
           -> Double  -- ^ The probability for any pair of nodes to be connected
           -> IO GraphContext -- ^ The resulting graph (IO required for randomness)
erdosRenyiContext gen n allNodes p = do
    let endpoints = selectWithProbability gen p allNodes
    inEdges <- endpoints
    outEdges <- endpoints
    return $ GraphContext inEdges n outEdges

{-|
    Generate a unlabelled directed random graph using the Algorithm introduced by
    Erdős and Rényi, also called a binomial random graph generator.

    Note that self-loops with also be generated with probability p.

    This algorithm runs in O(n²) and is best suited for non-sparse networks.

    The generated nodes are identified by [0..n-1].

    Example usage, using a truly random generator:
    
    > import System.Random.MWC
    > gen <- withSystemRandom . asGenIO $ return
    > erdosRenyiGraph 10 0.1
    ...

    Modelled after NetworkX 1.8.1 erdos_renyi_graph().
    
-}
erdosRenyiGraph :: GenIO  -- ^ The random number generator to use
           -> Int    -- ^ The number of nodes
           -> Double -- ^ The probability for any pair of nodes to be connected
           -> IO GraphInfo -- ^ The resulting graph (IO required for randomness)
erdosRenyiGraph gen n p = do
    let allNodes = [0..n-1]
    -- Outgoing edge targets for any node
    let outgoingEdgeTargets = selectWithProbability gen p allNodes
    -- Outgoing edge tuples for a single nodes
    let singleNodeEdges node = zip (repeat node) <$> outgoingEdgeTargets
    allEdges <- concat <$> mapM singleNodeEdges allNodes
    return $ GraphInfo n allEdges

{-|
    Like 'erdosRenyiGraph', but uses a newly initialized random number generator.

    See 'System.Random.MWC.withSystemRandom' for details on how the generator is
    initialized.

    By using this function, you don't have to initialize the generator by yourself,
    however generator initialization is slow, so reusing the generator is recommended.

    Usage example:

    > erdosRenyiGraph' 10 0.1
-}
erdosRenyiGraph' :: Int    -- ^ The number of nodes
                 -> Double -- ^ The probability for any pair of nodes to be connected
                 -> IO GraphInfo -- ^ The resulting graph (IO required for randomness)
erdosRenyiGraph' n p =
    withSystemRandom . asGenIO $ \gen -> erdosRenyiGraph gen n p

{-|
    Filter a list by selecting each list element
    uniformly with a given probability p

    Although this is mainly used internally, it can be used as general utility function
-}
selectWithProbability :: GenIO  -- ^ The random generator state
                      -> Double -- ^ The probability to select each list element
                      -> [a]    -- ^ The list to filter
                      -> IO [a] -- ^ The filtered list  
selectWithProbability _   _ [] = return []
selectWithProbability gen p (x:xs) = do
    r <- uniform gen :: IO Double
    let v = [x | r <= p]
    liftM2 (++) (return v) $ selectWithProbability gen p xs
