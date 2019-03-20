{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Sssp.Distance
    ( Distance
    , new
    , infinity
    , add
    ) where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)

import           Data.Word       (Word64)
import           Graph.Types     (Weight)

-- A node's distance from the source
data Distance = MkDistance Word64 | Infinity
    deriving (Eq, Generic, NFData)

instance Show Distance where
    show (MkDistance x) = show x
    show Infinity       = "infinity"

instance Ord Distance where
    MkDistance x <= MkDistance y = x <= y
    Infinity <= _ = False
    _ <= Infinity = True

new :: Integral a => a -> Distance
new = MkDistance . fromIntegral

infinity :: Distance
infinity = Infinity

-- The sum of two distances
add :: Distance -> Distance -> Distance
add (MkDistance x) (MkDistance y) = MkDistance (x + y)
add Infinity _                    = Infinity
add _ Infinity                    = Infinity
