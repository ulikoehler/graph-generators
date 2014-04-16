{-
    Random graph generators using the generator algorithm
    introduced by A. L. Barabási and R. Albert.

    See.
    A. L. Barabási and R. Albert "Emergence of scaling in
       random networks", Science 286, pp 509-512, 1999.
-}
module Data.Graph.Random.BarabasiAlbert (
    ) where

import System.Random.MWC
import System.Random.MWC.CondensedTable
import Data.Graph.Inductive
import GHC.Word (Word32)
import Control.Arrow (second)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMultiSet (IntMultiSet)
import Data.Vector.Generic (Vector)
import Data.Vector.Unboxed (fromList)
import qualified Data.IntMultiSet as IntMultiSet

-- | Select the nth element from a multiset occur list
selectNth :: Int -> [(Int, Int)] -> Int
selectNth _ [] = error "Can't select nth element - n is greater than list size"
selectNth n ((a,c):xs)
    | n <= c = a
    | otherwise = selectNth (n-c) xs

-- | Select n distinct random element from the multiset, with precalculated size
selectRandomElement :: GenIO -> (IntMultiSet, Int) -> IO Int
selectRandomElement gen (ms, msSize) = do
    let msOccurList = IntMultiSet.toOccurList ms
    r <- uniformR (0, msSize - 1) gen
    return $ selectNth r msOccurList

-- | Convert an occur list to a weight vector in the same order (but losing the element information)
--   This is required in order to 
occurListToTable :: [(Int, Int)] -> CondensedTableU Int
occurListToTable = 
    let toW32List :: [(Int, Int)] -> [(Int, Word32)]
        toW32List = map (second fromIntegral)
    in tableFromIntWeights . fromList . toW32List

-- | Select n distinct random elements from a multiset, with
--   This function will fail to terminate if there are less than n distinct
--   elements in the multiset. This function accepts a multiset with
--   precomputed size for performance reasons
selectNDistinctRandomElements :: GenIO -> Int -> (IntMultiSet, Int) -> IO [Int]
selectNDistinctRandomElements gen n (ms, msSize) = undefined

-- | Internal recursive worker for selectNDistinctRandomElements
--selectNDistinctRandomElementsWorker :: GenIO -> Int -> (IntMultiSet, Int) -> IntSet -> IO (Set Int)
--selectNDistinctRandomElementsWorker gen n (ms, msSize) current = do
--    randomElement = selectRandomElement gen 



{-
    Generate a random undirected Barabasi

    Modeled after NetworkX 1.8.1 barabasi_albert_graph()
-}
barabasiAlbertGraph :: GenIO  -- ^ The random number generator to use
                    -> Int    -- ^ The overall number of nodes (n)
                    -> Int    -- ^ The number of edges to create between a new and existing nodes (m)
                    -> IO UGr -- ^ The resulting graph (IO required for randomness)
barabasiAlbertGraph gen n m = do
    let nodes = [1..n] -- Nodes [1..m]: Initial nodes
    --stateGen =  
    undefined