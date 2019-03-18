module SsspSpec where

import           Test.Hspec

import qualified Data.HashMap.Strict as HashMap

import qualified Graph
import qualified Sssp
import qualified Sssp.Distance       as Distance
import           Util                (pairMap, pairMap')

spec :: Spec
spec = describe "Sssp" $ do
    let numNodes = 6
    let numEdges = 7
    let adjLists = [[], [0], [1, 3, 4, 5], [], [3, 5], []]
    let weights =
            [ ((1, 0), 3)
            , ((2, 1), 2), ((2, 3), 4), ((2, 4), 1), ((2, 5), 7)
            , ((4, 3), 2), ((4, 5), 3)
            ]
    let graph = Graph.mkGraph' numNodes numEdges adjLists weights
    let source = Graph.mkNode 2

    let distExpected = HashMap.fromList $ fmap (pairMap Graph.mkNode Distance.new)
            [(0, 5), (1, 2), (2, 0), (3, 3), (4, 1), (5, 4)]

    let prevExpected = HashMap.fromList $ fmap (pairMap' Graph.mkNode)
            [(0, 1), (1, 2), (3, 4), (4, 2), (5, 4)]

    describe "dijkstra" $ do
        let (dist, prev) = Sssp.dijkstra graph source

        it "finds the shortest distance from the source for each node" $
            dist `shouldBe` distExpected
        it "finds the shortest path from the source for each node" $
            prev `shouldBe` prevExpected

    describe "bellmanFord" $ do
        let (dist, prev) = Sssp.bellmanFord graph source

        it "finds the shortest distance from the source for each node" $
            dist `shouldBe` distExpected
        it "finds the shortest path from the source for each node" $
            prev `shouldBe` prevExpected
