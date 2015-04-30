module Data.CompactMap
    ( CompactMap
    , fromList
    , toVector
    , lookup
    ) where

import qualified Data.Vector as V
import GHC.Exts (sortWith)
import Data.Foldable
import Prelude hiding (lookup)

data CompactMap k v = CompactMap (V.Vector v) (v -> k)

instance Show v => Show (CompactMap k v) where
    show = show . V.toList . getMap

instance Foldable (CompactMap k) where
    foldr f i = foldr f i . getMap
    foldMap f = foldMap f . getMap

getMap :: CompactMap k v -> V.Vector v
getMap (CompactMap lst _) = lst
{-# INLINE getMap #-}

fromList :: Ord k => [v] -> (v -> k) -> CompactMap k v
fromList lst f = CompactMap (V.fromList $ sortWith f lst) f

toVector :: CompactMap k v -> V.Vector v
toVector (CompactMap v _) = v

lookup :: Ord k => k -> CompactMap k v -> Maybe v
lookup k (CompactMap lst f) = go 0 (sz - 1)
    where
        sz = V.length lst
        go l h
            | m < l     = Nothing
            | m > h     = Nothing
            | k' == k   = Just x
            | l == h    = Nothing
            | k' > k    = go l (m - 1)
            | otherwise = go (m + 1) h
            where
                m = (l + h) `div` 2
                x = lst V.! m
                k' = f x

