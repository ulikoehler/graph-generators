{-
  Functions to convert graph-generators 'Data.Graph.Generators.GraphInfo'
  to FGL data structures.

  Copyright (C) 2014 Uli Köhler
  Apache License v2.0
-}
module Data.Graph.Generators.FGL (
        graphInfoToUGr
    ) where

import Data.Graph.Generators
import Data.Graph.Inductive

graphInfoToUGr :: GraphInfo -- ^ The graph to convert
               -> UGr       -- ^ The resulting FGL graph
graphInfoToUGr (GraphInfo n edges) =
    let nodes = [0..n-1]
    in mkUGraph nodes edges