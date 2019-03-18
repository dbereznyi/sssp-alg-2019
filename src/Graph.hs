{-# LANGUAGE OverloadedStrings #-}

-- Top-level module
module Graph
    ( fromText
    , fromFile
    , toText
    , toFile
    , module Graph.Types
    ) where

import           Text.Parsec         (runParserT)

import           Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HashMap
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import qualified Data.Vector         as V

import           Control.Monad.ST    (runST)

import           Graph.Parser
import           Graph.Types

fromText :: Text -> Graph
fromText contents =
    case result of
        Left err    -> error (show err)
        Right graph -> graph
    where
        result = runST $ runParserT graphParser HashMap.empty "" contents

fromFile :: FilePath -> IO Graph
fromFile filePath = fromText <$> Text.readFile filePath

toText :: Graph -> Text
toText graph = Text.concat [header, lines]
    where
        header :: Text
        header = Text.pack $
            show (numNodes graph) <> ", " <> show (numEdges graph) <> "\n"

        lines :: Text
        lines = Text.dropEnd 1 $ Text.concat $
            uncurry toLine <$> zip [0..] (V.toList $ adjLists graph)

        toLine :: Int -> V.Vector Node -> Text
        toLine i adjList = Text.dropEnd 1 (Text.concat (toNodeWeight i <$> V.toList adjList)) <> "\n"

        toNodeWeight :: Int -> Node -> Text
        toNodeWeight i node = Text.pack $
            show node <> " "  <> show (weights graph ! mkEdge (mkNode i) node) <> " "

toFile :: FilePath -> Graph -> IO ()
toFile filePath graph = Text.writeFile filePath (toText graph)
