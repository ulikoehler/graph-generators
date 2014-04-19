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

hasNumEdges :: Int -> GraphInfo -> Bool
hasNumEdges n = (==n) . numEdges

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
        nullGraph `shouldSatisfy` checkGraphInfo
  describe "Regular graphs" $ do
    it "should pass the integrity checks" $ do
        forM_ [0..25] $ \n -> do
            cycleGraph n `shouldSatisfy` checkGraphInfo
            starGraph n `shouldSatisfy` checkGraphInfo
            wheelGraph n `shouldSatisfy` checkGraphInfo
    describe "Complete graphs" $ do
        it "should pass the integrity checks" $ do
            forM_ [0..10] $ \n ->
                completeGraph n `shouldSatisfy` checkGraphInfo
        it "should have n*(n-1)/2 edges without selfloops" $ do
            forM_ [0..10] $ \n ->
                let graph = completeGraph n
                in numEdges graph `shouldBe` n*(n-1) `div` 2
        it "should have (n*(n-1) / 2) + n edges with selfloops" $ do
            --Note that the formula doesn't apply for n=1 which has 1 edge (0,0)
            numEdges (completeGraphWithSelfloops 0) `shouldBe` 0
            forM_ [1..10] $ \n ->
                let graph = completeGraphWithSelfloops n
                in numEdges graph `shouldBe` (n*(n-1) `div` 2) + n
    describe "Line graphs" $ do
        it "should pass the integrity checks" $ do
            forM_ [0..25] $ \n -> do
                lineGraph n `shouldSatisfy` checkGraphInfo
        it "should have n-1 edges" $ do
            lineGraph 0 `shouldSatisfy` hasNumEdges 0
            forM_ [1..25] $ \n ->
                lineGraph n `shouldSatisfy` hasNumEdges (n-1)
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