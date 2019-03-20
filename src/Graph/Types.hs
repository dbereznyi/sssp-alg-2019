{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Defines the weighted digraph used in the Single-source Shortest Path problem
module Graph.Types
    ( Node
    , mkNode

    , Weight
    , mkWeight

    , Edge
    , mkEdge
    , unwrapEdge

    , NodeCount
    , mkNodeCount

    , EdgeCount
    , mkEdgeCount

    , Graph
    , numNodes
    , numEdges
    , adjLists
    , weights
    , mkGraph
    , mkGraph'
    , nodeList
    , toEdgeWeight
    , edgeWeightPairs
    , neighborsOf
    , adjEdgeWeightPairs
    , weightOf
    ) where

import           Control.DeepSeq     (NFData)

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector         as V
import           Data.Word           (Word64)

import           Util                (for, pairMap, pairMap')

-- Type aliases

newtype Node = MkNode Word64
    deriving (Eq, Hashable, NFData)

instance Show Node where
    show (MkNode val) = show val

mkNode :: Integral a => a -> Node
mkNode = MkNode . fromIntegral

newtype Weight = MkWeight Word64
    deriving (Eq, Hashable, Num, Ord, Real, Integral, Enum)

instance Show Weight where
    show (MkWeight val) = show val

mkWeight :: Integral a => a -> Weight
mkWeight = MkWeight . fromIntegral

newtype Edge = MkEdge (Node, Node)
    deriving (Eq, Hashable)

instance Show Edge where
    show (MkEdge (u, v)) = "(" ++ show u ++ ", " ++ show v ++ ")"

mkEdge :: Node -> Node -> Edge
mkEdge = curry MkEdge

unwrapEdge :: Edge -> (Node, Node)
unwrapEdge (MkEdge pair) = pair

newtype NodeCount = MkNodeCount Word64
    deriving (Eq, Num, Ord, Real, Integral, Enum)

instance Show NodeCount where
    show (MkNodeCount val) = show val

mkNodeCount :: Integral a => a -> NodeCount
mkNodeCount = MkNodeCount . fromIntegral

newtype EdgeCount = MkEdgeCount Word64
    deriving (Eq, Num, Ord, Real, Integral, Enum)

instance Show EdgeCount where
    show (MkEdgeCount val) = show val

mkEdgeCount :: Integral a => a -> EdgeCount
mkEdgeCount = MkEdgeCount . fromIntegral

-- The weighted digraph type

data Graph = MkGraph
    { numNodes :: NodeCount
    , numEdges :: EdgeCount
    , adjLists :: V.Vector (V.Vector Node)
    , weights  :: HashMap Edge Weight
    } deriving (Show, Eq)

mkGraph :: NodeCount -> EdgeCount -> V.Vector (V.Vector Node) -> HashMap Edge Weight -> Graph
mkGraph = MkGraph

-- Constructs a Graph from Integral values and plain lists
-- Convenient for constructing instances in code to avoid "mkX" boilerplate
mkGraph' :: Integral a => a -> a -> [[a]] -> [((a, a), a)] -> Graph
mkGraph' numNodes' numEdges' adjLists' weights' =
    mkGraph
        (mkNodeCount numNodes')
        (mkEdgeCount numEdges')
        (toAdjLists adjLists')
        (toWeights weights')
    where
        toAdjLists :: Integral a => [[a]] -> V.Vector (V.Vector Node)
        toAdjLists = V.fromList . fmap (V.fromList . fmap mkNode)

        toWeights :: Integral a => [((a, a), a)] -> HashMap Edge Weight
        toWeights = HashMap.fromList
            . fmap (pairMap (uncurry mkEdge . pairMap' mkNode) mkWeight)

-- A list of all the nodes in the graph (from 0 to numNodes - 1)
nodeList :: Graph -> [Node]
nodeList graph = fmap mkNode [0 .. nodeCount - 1]
    where
        MkNodeCount nodeCount = numNodes graph

-- Converts a pair of Nodes to an Edge paired with its Weight
toEdgeWeight :: Graph -> Node -> Node -> (Edge, Weight)
toEdgeWeight graph u v = let edge = mkEdge u v in (edge, weights graph HashMap.! edge)

-- A vector of every edge in the graph paired with its weight
-- Convenient for iterating over all the edges in the graph
edgeWeightPairs :: Graph -> V.Vector (Edge, Weight)
edgeWeightPairs graph =
    V.concatMap (\(u, vs) -> toEdgeWeight graph (mkNode u) <$> vs) (V.indexed $ adjLists graph)

-- The nodes reachable from a given node
neighborsOf :: Node -> Graph -> V.Vector Node
neighborsOf (MkNode nodeIdx) graph = adjLists graph V.! fromIntegral nodeIdx

-- Edges adjacent to the current node (i.e. can be travelled along from the current node)
-- paired with their weights
-- Convenient for iterating over edges to adjacent nodes
adjEdgeWeightPairs :: Node -> Graph -> V.Vector (Edge, Weight)
adjEdgeWeightPairs u graph = fmap (toEdgeWeight graph u) (neighborsOf u graph)

weightOf :: Edge -> Graph -> Weight
weightOf edge graph = weights graph HashMap.! edge
