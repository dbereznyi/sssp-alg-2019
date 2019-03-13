-- Parsing graphs from text
module Graph.Parser
    ( graphParser
    ) where

import           Graph.Type                (Edge, Graph, Node, Weight)
import qualified Graph.Type                as Graph

import           Text.Parsec

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Int                  (Int64)
import           Data.Text                 (Text)
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import           Data.Word                 (Word64)

import           Control.Monad             (forM_, void)
import           Control.Monad.ST          (ST)
import           Control.Monad.Trans.Class (lift)

-- Converts a text sequence in the graph file format to a graph
graphParser :: ParsecT Text (HashMap Edge Weight) (ST s) Graph
graphParser = do
    (numNodes, numEdges) <- headerP

    adjLists <- lift (VM.new numNodes)
    forM_ [0 .. numNodes - 1] $ \i ->
        lineP i adjLists

    adjListsImm <- lift (V.unsafeFreeze adjLists)
    weights <- getState

    return $
        Graph.mkGraph
            (Graph.mkNodeCount numNodes)
            (Graph.mkEdgeCount numEdges)
            adjListsImm
            weights

    where
        intP :: ParsecT Text u (ST s) Int
        intP = read <$> many digit

        headerP :: ParsecT Text (HashMap Edge Weight) (ST s) (Int, Int)
        headerP = do
            numNodes <- intP
            char ','
            space
            numEdges <- intP
            endOfLine
            return (numNodes, numEdges)

        nodeWeightP :: ParsecT Text (HashMap Edge Weight) (ST s) (Node, Weight)
        nodeWeightP = do
            node <- Graph.mkNode <$> intP
            space
            weight <- Graph.mkWeight <$> intP
            skipMany (char ' ')
            return (node, weight)

        lineP :: Int -> VM.STVector s (V.Vector Node) -> ParsecT Text (HashMap Edge Weight) (ST s) ()
        lineP i adjLists = do
            nodeWeightPairs <- manyTill nodeWeightP (void endOfLine <|> eof)

            -- Write the adj list
            let adjList = V.fromList (map fst nodeWeightPairs) :: V.Vector Node
            lift (VM.write adjLists i adjList)

            -- Insert the edge weights
            forM_ nodeWeightPairs $ \(node, weight) -> do
                let edge = Graph.mkEdge (Graph.mkNode i) node
                modifyState (HashMap.insert edge weight)

