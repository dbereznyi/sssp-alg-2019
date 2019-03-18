{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import           Data.Word           (Word64)

import           Graph               (Graph, Node, Weight)
import qualified Graph

import           Util                (for, insertAll, insertAllMQ)

-- A node's distance from the source
data Distance = MkDistance Word64 | Infinity
    deriving (Eq)

instance Show Distance where
    show (MkDistance x) = show x
    show Infinity       = "infinity"

instance Ord Distance where
    MkDistance x <= MkDistance y = x <= y
    Infinity <= _ = False
    _ <= Infinity = True

-- Converts a weight to a distance
fromWeight :: Weight -> Distance
fromWeight weight = MkDistance (fromIntegral weight)

-- The sum of two distances
distSum :: Distance -> Distance -> Distance
distSum (MkDistance x) (MkDistance y) = MkDistance (x + y)
distSum Infinity _                    = Infinity
distSum _ Infinity                    = Infinity

-- A node indexed by its distance from the source
-- Used as the element of the minimum priority queue in Dijkstra's algorithm
newtype NodeDist = MkNodeDist (Node, Distance)
    deriving (Eq, Show)

instance Ord NodeDist where
    MkNodeDist (_, distX) <= MkNodeDist (_, distY) = distX <= distY

dijkstra :: Graph -> Node -> (HashMap Node Distance, HashMap Node Node)
dijkstra graph source = go unvisitedInit distInit prevInit
    where
        -- The initial distance estimate for each node
        -- The distance of the source is set to 0, and every other node is set to Infinity
        nodeDistPairsInit :: [(Node, Distance)]
        nodeDistPairsInit = (source, MkDistance 0) : zip nonSourceNodes (repeat Infinity)
            where
                nonSourceNodes = filter (/= source) (Graph.nodeList graph)

        -- A minimum priority queue of nodes indexed by their estimated distance from the source
        unvisitedInit :: MinQueue NodeDist
        unvisitedInit = MinQueue.fromList (fmap MkNodeDist nodeDistPairsInit)

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
                (MkNodeDist (u, uDist), unvisited') = MinQueue.deleteFindMin unvisited

                improvedEstimates :: V.Vector (Node, Distance)
                improvedEstimates = V.filter (\(v, newEst) -> newEst < dist HashMap.! v) $
                    for (Graph.neighborsOf u graph) $ \v ->
                        let edge = Graph.mkEdge u v
                            weight = Graph.weights graph HashMap.! edge
                            newEst = distSum uDist (fromWeight weight)
                        in (v, newEst)
                    
                -- Update the distance estimates for u's neighbors
                unvisited'' :: MinQueue NodeDist
                unvisited'' = insertAllMQ unvisited' (fmap MkNodeDist improvedEstimates)

                -- dist updated with the improved distance estimates
                dist' :: HashMap Node Distance
                dist' = insertAll dist improvedEstimates

                -- The current node is the predecessor for every neighbor whose
                -- distance estimate was improved
                prev' :: HashMap Node Node
                prev' = insertAll prev $ fmap (\(v, _) -> (v, u)) improvedEstimates

bellmanFord :: Graph -> Node -> (HashMap Node Distance, HashMap Node Node)
bellmanFord graph source = undefined
