module SsspSpec where

import           Test.Hspec

import qualified Graph
import           Sssp       (Distance, dijkstra)

spec :: Spec
spec = describe "Sssp" $ do
    describe "dijkstra" $ do
        it "solves the SSSP problem" $
            undefined -- TODO
