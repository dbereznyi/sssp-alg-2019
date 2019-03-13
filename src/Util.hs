module Util
    ( pairMap
    , pairMap'
    , for
    , insertAll
    ) where

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

pairMap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
pairMap f g (x, y) = (f x, g y)

pairMap' :: (a -> b) -> (a, a) -> (b, b)
pairMap' f = pairMap f f

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

-- Insert multiple key/value pairs into a HashMap
insertAll :: (Eq k, Hashable k, Foldable f) => HashMap k v -> f (k, v) -> HashMap k v
insertAll = foldr (\(k, v) -> HashMap.insert k v)
