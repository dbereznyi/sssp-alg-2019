{-# LANGUAGE OverloadedStrings #-}

module Graph.ParserSpec where

import           Test.Hspec
import           Text.Parsec         (runParserT)

import qualified Data.HashMap.Strict as HashMap

import           Control.Monad.ST    (runST)

import           Graph.Parser        (graphParser)
import qualified Graph

spec :: Spec
spec = describe "Graph.Parser" $ do
    describe "graphParser" $ do
        let txt = "4, 5\n1 1\n2 2\n3 3\n0 4 1 5"
        let result = runST $ runParserT graphParser HashMap.empty "" txt

        let numNodes' = 4
        let numEdges' = 5
        let adjLists' = [[1], [2], [3], [0, 1]]
        let weights' = [((0, 1), 1), ((1, 2), 2), ((2, 3), 3), ((3, 0), 4), ((3, 1), 5)]

        let graph = Graph.mkGraph' numNodes' numEdges' adjLists' weights'

        it "parses the graph from text" $
            result `shouldBe` Right graph
