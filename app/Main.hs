module Main where

import qualified Criterion.Main      as Crit

import           Control.DeepSeq     (NFData, deepseq, force, rnf)
import           Control.Exception   (evaluate)
import           Control.Monad       (forM, forM_, replicateM)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import           Text.Printf         (printf)

import           System.CPUTime      (getCPUTime)
import           System.Random       (randomRIO)

import           Graph               (Graph, Node, NodeCount)
import qualified Graph
import           Graph.Generator     (genGraph)

import qualified Sssp
import           Sssp.Distance       (Distance)

import           Util                (pairMap)

graphsDir :: FilePath
graphsDir = "graphs/"

toGraphPath :: Graph.NodeCount -> FilePath
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

-- runExperiments :: [(Graph, Node)] -> IO ()
-- runExperiments inputs = forM_ inputs $ \(g, s) -> do
--     printf "Input: %s nodes, %s edges\n" (show $ Graph.numNodes g) (show $ Graph.numEdges g)

--     printf "\tDijkstra:\t"
--     time (Sssp.dijkstra g) s >>= evaluate . force

--     printf "\tBellman:\t"
--     time (Sssp.bellmanFord g) s >>= evaluate . force

-- time :: NFData b => (a -> b) -> a -> IO [b]
-- time f x = do
--     start <- getCPUTime
--     results <- replicateM 100000 $ evaluate $ force $ (f x)
--     end <- getCPUTime
--     let diff = (fromIntegral (end - start)) / (10^3)
--     printf "%0.6f nanosec\n" (diff / 100000 :: Double)
--     return results

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
    graph <- genGraph numNodes connectivity
    Graph.toFile (toGraphPath numNodes) graph
    where
        connectivity = 0.05
