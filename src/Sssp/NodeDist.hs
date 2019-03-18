module Sssp.NodeDist
    ( NodeDist(..)
    ) where

import           Graph         (Node)
import           Sssp.Distance (Distance)

-- A node indexed by its distance from the source
-- Used as the element of the minimum priority queue in Dijkstra's algorithm
newtype NodeDist = Mk (Node, Distance)
    deriving (Eq, Show)

instance Ord NodeDist where
    Mk (_, distX) <= Mk (_, distY) = distX <= distY