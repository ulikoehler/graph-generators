{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import System.Environment
import System.Directory
import Data.Graph.Generators(GraphInfo(..))
import Data.Graph.Generators.Random.ErdosRenyi(erdosRenyiGraph)
import Data.Graph.Generators.Random.WattsStrogatz(wattsStrogatzGraph)
import Data.Graph.Generators.Random.BarabasiAlbert(barabasiAlbertGraph)
import System.Random.MWC(createSystemRandom)
import System.IO

d  = 20
n  = 1000
p  = toDbl d / toDbl n
b  = 0.28
m0 = 10

toDbl = fromInteger.toInteger

sampleDir = "dist/build/samples"

writeGraph :: FilePath -> GraphInfo -> IO ()
writeGraph path (GraphInfo{..}) =
  withFile path WriteMode
  (\handle -> do
      hPutStrLn handle "strict graph {"
      mapM (hPutStrLn handle) [ (show x) ++ " -- " ++ (show y)
                              | (x, y) <- edges ]
      hPutStrLn handle "}"
  )

main :: IO ()
main = do
  args <- getArgs
  gen <- createSystemRandom
  eG <- erdosRenyiGraph gen n p
  wG <- wattsStrogatzGraph gen n d b
  bG <- barabasiAlbertGraph gen n d
  createDirectoryIfMissing True "dist/build/samples"
  writeGraph (sampleDir ++ "/ErdosRenyiGraph.dot") eG
  writeGraph (sampleDir ++ "/WattsStrogatz.dot") wG
  writeGraph (sampleDir ++ "/BarabasiAlbertGraph.dot") bG
  putStrLn "hello World."


