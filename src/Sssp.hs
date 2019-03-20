-- Algorithms for solving the Single-source Shortest Path problem
module Sssp
    ( dijkstra
    , bellmanFord
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
dijkstra graph source = go unvisitedInit distInit HashMap.empty
    where
        -- A minimum priority queue of nodes indexed by their estimated distance from the source
        unvisitedInit :: MinQueue NodeDist
        unvisitedInit = MinQueue.fromList (fmap NodeDist.Mk (nodeDistPairsInit graph source))

        distInit :: HashMap Node Distance
        distInit = HashMap.fromList (nodeDistPairsInit graph source)

        go :: MinQueue NodeDist
            -> HashMap Node Distance
            -> HashMap Node Node
            -> (HashMap Node Distance, HashMap Node Node)
        go unvisited dist prev =
            if MinQueue.null unvisited
                then (dist, prev)
                else go unvisited'' dist' prev'
            where
                -- The node with the lowest distance estimate is dequeued
                (NodeDist.Mk (u, _), unvisited') = MinQueue.deleteFindMin unvisited

                -- Updates to distance estimates and predecessors
                updates :: V.Vector ((Node, Distance), (Node, Node))
                updates = V.mapMaybe (relax dist) (Graph.adjEdgeWeightPairs u graph)

                -- Apply the updates
                unvisited'' = insertAllMQ unvisited' (fmap (NodeDist.Mk . fst) updates)
                dist' = insertAll dist (fmap fst updates)
                prev' = insertAll prev (fmap snd updates)

bellmanFord :: Graph -> Node -> (HashMap Node Distance, HashMap Node Node)
bellmanFord graph source = go 1 distInit HashMap.empty
    where
        distInit :: HashMap Node Distance
        distInit = HashMap.fromList (nodeDistPairsInit graph source)

        go :: Int
            -> HashMap Node Distance
            -> HashMap Node Node
            -> (HashMap Node Distance, HashMap Node Node)
        go i dist prev =
            if i == fromIntegral (Graph.numNodes graph)
                then (dist, prev)
                else go (i + 1) dist' prev'
            where
                -- Updates to distance estimates and predecessors
                updates :: V.Vector ((Node, Distance), (Node, Node))
                updates = V.mapMaybe (relax dist) (Graph.edgeWeightPairs graph)

                -- Apply the updates
                dist' = insertAll dist (fmap fst updates)
                prev' = insertAll prev (fmap snd updates)

-- The initial distance estimate for each node
-- The distance of the source is set to 0, and every other node is set to Infinity
nodeDistPairsInit :: Graph -> Node -> [(Node, Distance)]
nodeDistPairsInit graph source =
    (source, Distance.new 0) : zip nonSourceNodes (repeat Distance.infinity)
    where
        nonSourceNodes = filter (/= source) (Graph.nodeList graph)

-- Tries to improve the distance estimate for the endpoint of the given edge
relax :: HashMap Node Distance -> (Edge, Weight) -> Maybe ((Node, Distance), (Node, Node))
relax dist (edge, weight) =
    if newEst < vDist
        then Just ((v, newEst), (v, u))
        else Nothing
    where
        (u, v) = Graph.unwrapEdge edge
        (uDist, vDist) = (dist HashMap.! u, dist HashMap.! v)
        newEst = Distance.add uDist (Distance.new weight)
