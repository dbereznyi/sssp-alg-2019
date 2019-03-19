-- Algorithms for solving the Single-source Shortest Path problem
module Sssp
    ( dijkstra
    , bellmanFord
    , Distance(..)
    ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List           (repeat)
import           Data.PQueue.Min     (MinQueue)
import qualified Data.PQueue.Min     as MinQueue
import qualified Data.Vector         as V

import           Graph               (Edge, Graph, Node, Weight)
import qualified Graph

import           Util                (for, insertAll, insertAllMQ, pairMap')

import           Sssp.Distance       (Distance)
import qualified Sssp.Distance       as Distance
import           Sssp.NodeDist       (NodeDist)
import qualified Sssp.NodeDist       as NodeDist

dijkstra :: Graph -> Node -> (HashMap Node Distance, HashMap Node Node)
dijkstra graph source = go unvisitedInit distInit prevInit
    where
        -- The initial distance estimate for each node
        -- The distance of the source is set to 0, and every other node is set to Infinity
        nodeDistPairsInit :: [(Node, Distance)]
        nodeDistPairsInit = (source, Distance.new 0) : zip nonSourceNodes (repeat Distance.infinity)
            where
                nonSourceNodes = filter (/= source) (Graph.nodeList graph)

        -- A minimum priority queue of nodes indexed by their estimated distance from the source
        unvisitedInit :: MinQueue NodeDist
        unvisitedInit = MinQueue.fromList (fmap NodeDist.Mk nodeDistPairsInit)

        distInit :: HashMap Node Distance
        distInit = HashMap.fromList nodeDistPairsInit

        prevInit :: HashMap Node Node
        prevInit = HashMap.empty

        go :: MinQueue NodeDist
            -> HashMap Node Distance
            -> HashMap Node Node
            -> (HashMap Node Distance, HashMap Node Node)
        go unvisited dist prev =
            if MinQueue.null unvisited
                then (dist, prev)
                else go unvisited'' dist' prev'
            where
                -- Dequeue the node with the lowest distance estimate
                (NodeDist.Mk (u, uDist), unvisited') = MinQueue.deleteFindMin unvisited

                improvedEstimates :: V.Vector (Node, Distance)
                improvedEstimates = V.filter (\(v, newEst) -> newEst < dist HashMap.! v) $
                    for (Graph.neighborsOf u graph) $ \v ->
                        let edge = Graph.mkEdge u v
                            weight = Graph.weights graph HashMap.! edge
                            newEst = Distance.add uDist (Distance.new weight)
                        in (v, newEst)

                -- Update the distance estimates for u's neighbors
                unvisited'' :: MinQueue NodeDist
                unvisited'' = insertAllMQ unvisited' (fmap NodeDist.Mk improvedEstimates)

                -- dist updated with the improved distance estimates
                dist' :: HashMap Node Distance
                dist' = insertAll dist improvedEstimates

                -- The current node is the predecessor for every neighbor whose
                -- distance estimate was improved
                prev' :: HashMap Node Node
                prev' = insertAll prev $ fmap (\(v, _) -> (v, u)) improvedEstimates

bellmanFord :: Graph -> Node -> (HashMap Node Distance, HashMap Node Node)
bellmanFord graph source = go 1 distInit prevInit
    where
        -- The initial distance estimate for each node
        -- The distance of the source is set to 0, and every other node is set to Infinity
        distInit :: HashMap Node Distance
        distInit = HashMap.fromList $
            (source, Distance.new 0) : zip nonSourceNodes (repeat Distance.infinity)
            where
                nonSourceNodes = filter (/= source) (Graph.nodeList graph)

        prevInit :: HashMap Node Node
        prevInit = HashMap.empty

        go :: Int
            -> HashMap Node Distance
            -> HashMap Node Node
            -> (HashMap Node Distance, HashMap Node Node)
        go i dist prev =
            if i == fromIntegral (Graph.numNodes graph)
                then (dist, prev)
                else go (i + 1) dist' prev'
            where
                relax :: (Edge, Weight) -> Maybe ((Node, Distance), (Node, Node))
                relax (edge, weight) =
                    if newEst < vDist
                        then Just ((v, newEst), (v, u))
                        else Nothing
                    where
                        (u, v) = Graph.unwrapEdge edge
                        (uDist, vDist) = (dist HashMap.! u, dist HashMap.! v)
                        newEst = Distance.add uDist (Distance.new weight)

                updates = V.mapMaybe relax (Graph.edgeWeightPairs graph)
                
                dist' = insertAll dist (fmap fst updates)
                prev' = insertAll prev (fmap snd updates)

