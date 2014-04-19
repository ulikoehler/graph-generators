import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad
import Data.Graph.Generators.Classic
import Data.Graph.Generators.Regular
import Data.Graph.Generators.Random.ErdosRenyi
import Data.Graph.Generators.Random.BarabasiAlbert
import Data.Graph.Generators
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.List (sort)
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
  describe "Classic graphs" $ do
    it "should pass the integity check" $ do
        trivialGraph `shouldSatisfy` checkGraphInfo
        bullGraph `shouldSatisfy` checkGraphInfo
        chvatalGraph `shouldSatisfy` checkGraphInfo
        cubicalGraph `shouldSatisfy` checkGraphInfo
        desarguesGraph `shouldSatisfy` checkGraphInfo
        diamondGraph `shouldSatisfy` checkGraphInfo
        dodecahedralGraph `shouldSatisfy` checkGraphInfo
        fruchtGraph `shouldSatisfy` checkGraphInfo
        heawoodGraph `shouldSatisfy` checkGraphInfo
        houseGraph `shouldSatisfy` checkGraphInfo
        houseXGraph `shouldSatisfy` checkGraphInfo
        icosahedralGraph `shouldSatisfy` checkGraphInfo
        krackhardtKiteGraph `shouldSatisfy` checkGraphInfo
        moebiusKantorGraph `shouldSatisfy` checkGraphInfo
        octahedralGraph `shouldSatisfy` checkGraphInfo
        pappusGraph `shouldSatisfy` checkGraphInfo
        petersenGraph `shouldSatisfy` checkGraphInfo
        sedgewickMazeGraph `shouldSatisfy` checkGraphInfo
        tetrahedralGraph `shouldSatisfy` checkGraphInfo
        truncatedCubeGraph `shouldSatisfy` checkGraphInfo
        truncatedTetrahedronGraph `shouldSatisfy` checkGraphInfo
        tutteGraph `shouldSatisfy` checkGraphInfo
  describe "Complete graphs" $ do
    it "should pass the integrity checks" $ do
        forM_ [0..10] $ \n ->
            completeGraph n `shouldSatisfy` checkGraphInfo
    --it "should have n^2/2 edges" $ do
  describe "Regular graphs" $ do
    it "should pass the integrity checks" $ do
        forM_ [0..25] $ \n -> 
            lineGraph n `shouldSatisfy` checkGraphInfo
  describe "Erdös Renyi random graphs" $ do
    it "should pass the integrity checks" $ do
        forM_ [0..20] $ \n -> do
            gr <- erdosRenyiGraph' n 0.1
            completeGraph n `shouldSatisfy` checkGraphInfo
  describe "Barabasi Albert random graphs" $ do
    it "should pass the integrity checks" $ do
        forM_ [10..20] $ \n -> do
            gr <- barabasiAlbertGraph' n 5
            completeGraph n `shouldSatisfy` checkGraphInfo