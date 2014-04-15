
module Data.Graph.Random (
    )
    where

import System.Random.MWC
import Control.Monad
import Data.Graph.Inductive
import Control.Applicative ((<$>))

asDouble :: Double -> Double
asDouble = id

{-
    Filter a list by selecting each list element
    uniformly with a given probability p
-}
selectWithProbability :: GenIO  -- ^ The random generator state
                      -> Double -- ^ The probability to select each list element
                      -> [a]    -- ^ The list to filter
                      -> IO [a] -- ^ The filtered list  
selectWithProbability gen p [] = return []
selectWithProbability gen p (x:xs) = do
    r <- uniform gen :: IO Double
    let v = if r <= p then [x] else []
    (liftM2 (++)) (return v) $ selectWithProbability gen p xs

{-
    Generate a unlabelled context using the Erdős and Rényi method.

    See 'erdosRenyiGraph' for a detailed algorithm description.

    Example usage, using a truly random generator:
    
    > import System.Random.MWC
    > gen <- withSystemRandom . asGenIO $ return
    > 
-}
erdosRenyiContext :: GenIO  -- ^ The random number generator to use
           -> Node    -- ^ Identifier of the context's central node
           -> [Node]  -- ^ The algorithm will generate random edges to those nodes
                     --   from or to the given node
           -> Double -- ^ The probability for any pair of nodes to be connected
           -> IO UContext -- ^ The resulting graph (IO required for randomness)
erdosRenyiContext gen n nodes p = do
    let endpoints = selectWithProbability gen p nodes
    inEdges <- endpoints
    outEdges <- endpoints
    return (inEdges, n, outEdges)

{-
    Generate a unlabelled random graph using the Algorithm introduced by
    Erdős and Rényi, also called a binomial random graph generator.

    This algorithm runs in O(n²) and is best suited for non-sparse networks.

    The generated nodes are identified by [1..n].

    Example usage, using a truly random generator:
    
    > import System.Random.MWC
    > gen <- withSystemRandom . asGenIO $ return
    > erdosRenyiGraph 10 0.1
    ...

    Modelled after NetworkX 1.8.1 erdos_renyi_graph().
    See Erdős and A. Rényi, On Random Graphs, Publ. Math. 6, 290 (1959).
-}
erdosRenyiGraph :: GenIO  -- ^ The random number generator to use
           -> Int    -- ^ The number of nodes
           -> Double -- ^ The probability for any pair of nodes to be connected
           -> IO UGr -- ^ The resulting graph (IO required for randomness)
erdosRenyiGraph gen n p = do
    let nodes = [1..n]
    -- Outgoing edge targets for any node
    let outgoingEdgeTargets = selectWithProbability gen p nodes
    -- Outgoing edge tuples for a single nodes
    let edges n = zip (repeat n) <$> outgoingEdgeTargets
    allEdges <- concat <$> mapM edges nodes
    return $ mkUGraph nodes allEdges

{-
    Like 'erdosRenyiGraph', but uses a fresh random number generator.

    By using this function, you don't have to initialize the generator by yourself,
    however generator initialization is slow, so reusing the generator is recommended.

    Example usage:

    > erdosRenyiGraph' 10 0.1
-}
erdosRenyiGraph' :: Int    -- ^ The number of nodes
                 -> Double -- ^ The probability for any pair of nodes to be connected
                 -> IO UGr -- ^ The resulting graph (IO required for randomness)
erdosRenyiGraph' n p =
    withSystemRandom . asGenIO $ \gen -> erdosRenyiGraph gen n p