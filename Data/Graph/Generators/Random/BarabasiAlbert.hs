{-|
    Random graph generators using the generator algorithm
    introduced by A. L. Barabási and R. Albert.

    See.
    A. L. Barabási and R. Albert "Emergence of scaling in
       random networks", Science 286, pp 509-512, 1999.
-}
module Data.Graph.Generators.Random.BarabasiAlbert (
        -- ** Graph generators
        barabasiAlbertGraph,
        barabasiAlbertGraph',
        -- ** Utility functions
        selectNth,
        selectRandomElement,
        selectNDistinctRandomElements
    ) where

import Control.Monad
import Data.List (foldl')
import System.Random.MWC
import Data.Graph.Generators
import Control.Applicative
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMultiSet (IntMultiSet)
import Debug.Trace
import qualified Data.IntMultiSet as IntMultiSet

-- | Select the nth element from a multiset occur list, treating it as virtual large list
--   This is significantly faster than building up the entire list and selecting the nth
--   element
selectNth :: Int -> [(Int, Int)] -> Int
selectNth n [] = error $ "Can't select nth element - n is greater than list size (n=" ++ show n ++ ", list empty)"
selectNth n ((a,c):xs)
    | n <= c = a
    | otherwise = selectNth (n-c) xs

-- | Select a single random element from the multiset, with precalculated size
--   Note that the given size must be the total multiset size, not the number of
--   distinct elements in said se
selectRandomElement :: GenIO -> (IntMultiSet, Int) -> IO Int
selectRandomElement gen (ms, msSize) = do
    let msOccurList = IntMultiSet.toOccurList ms
    r <- uniformR (0, msSize - 1) gen
    return $ selectNth r msOccurList

-- | Select n distinct random elements from a multiset, with
--   This function will fail to terminate if there are less than n distinct
--   elements in the multiset. This function accepts a multiset with
--   precomputed size for performance reasons
selectNDistinctRandomElements :: GenIO -> Int -> (IntMultiSet, Int) -> IO [Int]
selectNDistinctRandomElements gen n t@(ms, msSize)
    | n == msSize = return . map fst . IntMultiSet.toOccurList $ ms
    | msSize < n = error "Can't select n elements from a set with less than n elements"
    | otherwise = IntSet.toList <$> selectNDistinctRandomElementsWorker gen n t IntSet.empty

-- | Internal recursive worker for selectNDistinctRandomElements
--   Precondition: n > num distinct elems in multiset (not checked).
--   Does not terminate if the precondition doesn't apply.
--   This implementation is quite naive and selects elements randomly until
--   the predefined number of elements are set.
selectNDistinctRandomElementsWorker :: GenIO -> Int -> (IntMultiSet, Int) -> IntSet -> IO IntSet
selectNDistinctRandomElementsWorker _ 0 _ current = return current
selectNDistinctRandomElementsWorker gen n t@(ms, msSize) current = do
        randomElement <- selectRandomElement gen t
        let currentWithRE = IntSet.insert randomElement current
        if randomElement `IntSet.member` current
            then selectNDistinctRandomElementsWorker gen n t current
            else selectNDistinctRandomElementsWorker gen (n-1) t currentWithRE


-- | Internal fold state for the Barabasi generator.
--   TODO: Remove this declaration from global namespace
type BarabasiState = (IntMultiSet, [Int], [(Int, Int)])

{-|
    Generate a random quasi-undirected Barabasi graph.

    Only one edge (with nondeterministic direction) is created between a node pair,
    because adding the other edge direction is easier than removing duplicates.

    Precondition (not checked): m <= n

    Modeled after NetworkX 1.8.1 barabasi_albert_graph()
-}
barabasiAlbertGraph :: GenIO  -- ^ The random number generator to use
                    -> Int    -- ^ The overall number of nodes (n)
                    -> Int    -- ^ The number of edges to create between a new and existing nodes (m)
                    -> IO GraphInfo -- ^ The resulting graph (IO required for randomness)
barabasiAlbertGraph gen n m = do
    -- Implementation concept: Iterate over nodes [m..n] in a state monad,
    --   building up the edge list
    -- Highly influenced by NetworkX barabasi_albert_graph()
    let nodes = [0..n-1] -- Nodes [0..m-1]: Initial nodes
     -- (Our state: repeated nodes, current targets, edges)
    let initState = (IntMultiSet.empty, [0..m-1], [])
    -- Strategy: Fold over the list, using a BarabasiState als fold state
    let folder :: BarabasiState -> Int -> IO BarabasiState
        folder st curNode = do
            let (repeatedNodes, targets, edges) = st
            -- Create new edges (for the current node)
            let newEdges = map (\t -> (curNode, t)) targets
            -- Add nodes to the repeated nodes multiset
            let newRepeatedNodes = foldl' (flip IntMultiSet.insert) repeatedNodes targets
            let newRepeatedNodes' = IntMultiSet.insertMany curNode m newRepeatedNodes
            -- Select the new target set randomly from the repeated nodes
            let repeatedNodesWithSize = (newRepeatedNodes, IntMultiSet.size newRepeatedNodes)
            newTargets <- selectNDistinctRandomElements gen m repeatedNodesWithSize
            return (newRepeatedNodes', newTargets, edges ++ newEdges)
    -- From the final state, we only require the edge list
    (_, _, allEdges) <- foldM folder initState [m..n-1]
    return $ GraphInfo n allEdges

{-|
    Like 'barabasiAlbertGraph', but uses a newly initialized random number generator.

    See 'System.Random.MWC.withSystemRandom' for details on how the generator is
    initialized.

    By using this function, you don't have to initialize the generator by yourself,
    however generator initialization is slow, so reusing the generator is recommended.

    Usage example:

    > barabasiAlbertGraph' 10 5
-}
barabasiAlbertGraph' :: Int    -- ^ The number of nodes
                     -> Int    -- ^ The number of edges to create between a new and existing nodes (m)
                     -> IO GraphInfo -- ^ The resulting graph (IO required for randomness)
barabasiAlbertGraph' n m =
    withSystemRandom . asGenIO $ \gen -> barabasiAlbertGraph gen n m