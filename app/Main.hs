module Main where

import           Control.Monad   (forM, forM_)

import           System.Random   (randomRIO)

import qualified Graph
import           Graph.Generator (genGraph)

import           Util            (pairMap)

graphsDir :: FilePath
graphsDir = "graphs/"

toGraphPath :: Graph.NodeCount -> FilePath
toGraphPath numNodes = graphsDir <> "graph_" <> show numNodes <> ".txt"

main :: IO ()
main = do
    inputs <- getInputs
    mapM_ (print . pairMap Graph.numNodes id) inputs

-- Reads input graphs and randomly selects a source vertex for each
getInputs :: IO [(Graph.Graph, Graph.Node)]
getInputs = forM [100,200..1000] $ \numNodes -> do
    graph <- Graph.fromFile (toGraphPath numNodes)
    source <- Graph.mkNode <$> randomRIO (0, fromIntegral $ numNodes - 1 :: Int)
    return (graph, source)

-- Generates 10 graphs ranging from 100 to 1000 nodes in size and write them to disk
-- Only needs to be called once
generateGraphs :: IO ()
generateGraphs = forM_ [100,200..1000] $ \numNodes -> do
    graph <- genGraph numNodes connectivity
    Graph.toFile (toGraphPath numNodes) graph
    where
        connectivity = 0.05
