{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph.Generator
    ( Connectivity
    , mkConnectivity
    , genGraph
    ) where


import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.IORef          (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM
import           Data.Word           (Word64)

import           Control.Monad       (forM_, when)

import           System.Random       (randomIO, randomRIO)

import           Graph.Type          (Edge, Graph, Node, NodeCount, Weight)
import qualified Graph.Type          as Graph

newtype Connectivity = MkConnectivity Double
    deriving (Eq, Num, Fractional, Floating, Real, RealFrac, Ord)

instance Show Connectivity where
    show (MkConnectivity val) = show val

genGraph :: NodeCount -> Connectivity -> IO Graph
genGraph numNodes connectivity = do
    adjLists <- VM.new (fromIntegral numNodes)
    forM_ [0 .. fromIntegral numNodes - 1] $ \i ->
        VM.write adjLists i V.empty
    let nodes = Set.fromList [0 .. fromIntegral numNodes]
    let current = 0
    let undisc = Set.delete current nodes
    weightsRef <- newIORef HashMap.empty

    connectGraph adjLists weightsRef undisc current

    forM_ [0 .. fromIntegral numNodes - 1] $ \src ->
        forM_ [0 .. fromIntegral numNodes - 1] $ \dst -> do
            r <- randomIO :: IO Double
            when (src /= dst && MkConnectivity r <= connectivity) $ do
                adjList <- VM.read adjLists src
                -- Only modify the adjList if this edge is not already there
                when (V.notElem (Graph.mkNode dst) adjList) $
                    VM.modify adjLists (\adjList -> V.snoc adjList (Graph.mkNode dst)) src

                -- Generate a weight for this edge
                let edge = Graph.mkEdge (Graph.mkNode src) (Graph.mkNode dst)
                addRandomWeight edge weightsRef

    adjListsImm <- V.unsafeFreeze adjLists
    let numEdges = Graph.mkEdgeCount $ sum (fmap V.length adjListsImm)

    weights <- readIORef weightsRef

    return (Graph.mkGraph numNodes numEdges adjListsImm weights)

    where
        addRandomWeight :: Edge -> IORef (HashMap Edge Weight) -> IO ()
        addRandomWeight edge weightsRef = do
            weightVal <- Graph.mkWeight <$> (randomRIO (1, 1000) :: IO Word64)
            modifyIORef' weightsRef (HashMap.insert edge weightVal)

        -- Ensures the resulting digraph is weakly connected
        -- SSSP can have unreachable nodes in the input graph
        connectGraph :: VM.IOVector (V.Vector Node)
            -> IORef (HashMap Edge Weight)
            -> HashSet Int
            -> Int
            -> IO ()
        connectGraph adjLists weightsRef undisc current = do
            -- toList is lazy, so only 1 elem is computed
            let target = head (Set.toList undisc)

            -- Place an edge from current to target
            VM.modify adjLists (\adjList -> V.snoc adjList (Graph.mkNode target)) current

            -- Generate a weight for this edge
            let edge = Graph.mkEdge (Graph.mkNode current) (Graph.mkNode target)
            addRandomWeight edge weightsRef

            let undisc' = Set.delete target undisc
            when (Set.null undisc') $
                connectGraph adjLists weightsRef undisc' target
