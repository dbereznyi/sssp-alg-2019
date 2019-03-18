{-# LANGUAGE OverloadedStrings #-}

module GraphSpec where

import           Test.Hspec

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.IO        as Text
import qualified Data.Vector         as V

import qualified Graph

spec :: Spec
spec = describe "Graph" $ do
    let numNodes = 4
    let numEdges = 5
    let adjLists = [[1], [2], [3], [0, 1]]
    let weights = [((0, 1), 1), ((1, 2), 2), ((2, 3), 3), ((3, 0), 4), ((3, 1), 5)]

    let graph = Graph.mkGraph' numNodes numEdges adjLists weights
    let txt = "4, 5\n1 1\n2 2\n3 3\n0 4 1 5"

    describe "fromText" $
        it "reads a graph file" $
            Graph.fromText txt `shouldBe` graph

    describe "toText" $
        it "converts a graph to text" $
            Graph.toText graph `shouldBe` txt

    describe "nodeList" $
        it "returns 0, 1, 2, 3" $
            Graph.nodeList graph `shouldBe` fmap Graph.mkNode [0..3]
