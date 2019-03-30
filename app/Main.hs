module Main where

import qualified Criterion.Main  as Crit

import           Control.Monad   (forM, forM_)

import           System.Random   (randomRIO)

import           Graph           (Graph, Node, NodeCount)
import qualified Graph
import           Graph.Generator (genGraph)

import qualified Sssp

graphsDir :: FilePath
graphsDir = "graphs/"

toGraphPath :: NodeCount -> FilePath
toGraphPath numNodes = graphsDir <> "graph_" <> show numNodes <> ".txt"

main :: IO ()
main = getInputs >>= Crit.defaultMain . fmap toBenchGroup

-- The benchmarks to run on the SSSP algorithms
-- There are 10 groups: one for each input size
-- In each group are two benchmarks: one for Dijkstra and one for Bellman Ford
toBenchGroup :: (Graph, Node) -> Crit.Benchmark
toBenchGroup (graph, source) =
    let numNodes = Graph.numNodes graph
        numEdges = Graph.numEdges graph
        groupName = show numNodes <> "N," <> show numEdges <> "E"
        toBench algName alg = Crit.bench algName $ Crit.nf (alg graph) source

    in Crit.bgroup groupName
        [ toBench "dijkstra" Sssp.dijkstra
        , toBench "bellmanFord" Sssp.bellmanFord
        ]

-- Reads input graphs and randomly selects a source vertex for each
getInputs :: IO [(Graph, Node)]
getInputs = forM [100,200..1000] $ \numNodes -> do
    graph <- Graph.fromFile (toGraphPath numNodes)
    source <- Graph.mkNode <$> randomRIO (0, fromIntegral $ numNodes - 1 :: Int)
    return (graph, source)

-- Generates 10 graphs ranging from 100 to 1000 nodes in size and writes them to disk
-- Only needs to be called once
generateGraphs :: IO ()
generateGraphs = forM_ [100,200..1000] $ \numNodes -> do
    let connectivity = 0.05
    graph <- genGraph numNodes connectivity
    Graph.toFile (toGraphPath numNodes) graph
