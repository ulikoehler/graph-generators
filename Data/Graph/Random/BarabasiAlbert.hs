{-
    Random graph generators using the generator algorithm
    introduced by A. L. Barabási and R. Albert.

    See.
    A. L. Barabási and R. Albert "Emergence of scaling in
       random networks", Science 286, pp 509-512, 1999.
-}
module Data.Graph.Random.BarabasiAlbert (
    ) where

import Control.Monad
import System.Random.MWC
import Control.Monad.State.Strict
import Data.Graph.Inductive
import Control.Applicative
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMultiSet (IntMultiSet)
import qualified Data.IntMultiSet as IntMultiSet

-- | Select the nth element from a multiset occur list, treating it as virtual large list
--   This is significantly faster than building up the entire list and selecting the nth
--   element
selectNth :: Int -> [(Int, Int)] -> Int
selectNth _ [] = error "Can't select nth element - n is greater than list size"
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
    | n < msSize = error "Can't select n elements from a set with less than n elements"
    | otherwise = IntSet.toList <$> selectNDistinctRandomElementsWorker gen n t IntSet.empty

-- | Internal recursive worker for selectNDistinctRandomElements
--   Precondition: n > num distinct elems in multiset (not checked)
--   This implementation is quite naive and selects elements randomly until
--   a pre
selectNDistinctRandomElementsWorker :: GenIO -> Int -> (IntMultiSet, Int) -> IntSet -> IO IntSet
selectNDistinctRandomElementsWorker _ 0 _ current = return current
selectNDistinctRandomElementsWorker gen n t@(ms, msSize) current = do
        randomElement <- selectRandomElement gen t
        let currentWithRE = IntSet.insert randomElement current
        if randomElement `IntSet.member` current
            then selectNDistinctRandomElementsWorker gen n t current
            else selectNDistinctRandomElementsWorker gen (n-1) t currentWithRE

{-
    Generate a random quasi-undirected Barabasi graph.

    Only one edge (with nondeterministic direction) is created between a node pair,
    because adding the other edge direction is easier than removing duplicates.

    Modeled after NetworkX 1.8.1 barabasi_albert_graph()
-}
barabasiAlbertGraph :: GenIO  -- ^ The random number generator to use
                    -> Int    -- ^ The overall number of nodes (n)
                    -> Int    -- ^ The number of edges to create between a new and existing nodes (m)
                    -> IO UGr -- ^ The resulting graph (IO required for randomness)
barabasiAlbertGraph gen n m = do
    -- Implementation concept: Iterate over nodes [m..n] in a state monad,
    --   building up the edge list
    -- Highly influenced by NetworkX barabasi_albert_graph()
    let nodes = [1..n] -- Nodes [1..m-1]: Initial nodes
     -- (Our state: repeated nodes, current targets, edges)
    let initState = (IntMultiSet.empty, [1..m-1], [])
    let fn = forM_ [m..n] $ \curNode -> do
            (repeatedNodes, targets, edges) <- get
            let msWithSize = (repeatedNodes, IntMultiSet.size repeatedNodes)
            newTargets <- liftIO $ selectNDistinctRandomElements gen m msWithSize
            -- Create all edges to add
            let newEdges = map (\t -> (curNode, t)) targets
            -- Create new list 
            let newRepeatedNodes = foldl' (flip IntMultiSet.insert) repeatedNodes targets
            let newRepeatedNodes' = IntMultiSet.insertMany curNode m newRepeatedNodes
            put (newRepeatedNodes', newTargets, edges ++ newEdges)
    x <- execState initState fn
    undefined
