{-# LANGUAGE Safe #-}

module Data.Graph.Random (
    )
    where

import System.Random.MWC
import Data.Graph

{-
    Generate a random graph using the 
-}
erdosRenyi :: GenIO -> Int -> Double -> IO (Gr () ())