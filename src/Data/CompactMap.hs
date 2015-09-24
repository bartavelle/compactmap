module Data.CompactMap
    ( CompactMap
    , fromList
    , toVector
    , lookup
    , getLE
    , getIndex
    ) where

import qualified Data.Vector as V
import GHC.Exts (sortWith)
import Data.Foldable as F
import Control.Applicative
import Prelude hiding (lookup)

data CompactMap k v = CompactMap (V.Vector v) (v -> k)

instance Show v => Show (CompactMap k v) where
    show = show . V.toList . getMap

instance Foldable (CompactMap k) where
    foldr f i = F.foldr f i . getMap
    foldMap f = F.foldMap f . getMap

getMap :: CompactMap k v -> V.Vector v
getMap (CompactMap lst _) = lst
{-# INLINE getMap #-}

fromList :: Ord k => [v] -> (v -> k) -> CompactMap k v
fromList lst f = CompactMap (V.fromList $ sortWith f lst) f

toVector :: CompactMap k v -> V.Vector v
toVector (CompactMap v _) = v

lookup :: Ord k => k -> CompactMap k v -> Maybe v
lookup k cm@(CompactMap _ f) = getLE k cm >>= \(_,r) -> if f r == k
                                                            then Just r
                                                            else Nothing

getLE :: Ord k => k -> CompactMap k v -> Maybe (Int, v)
getLE k (CompactMap lst f) = go 0 (sz - 1)
    where
        sz = V.length lst
        go l h
            | m < l     = Nothing
            | m > h     = Nothing
            | k' == k   = Just (m, x)
            | l >= h    = checkLower <|> Just (m, x)
            | k < k'    = checkLower <|> go l (m - 1)
            | otherwise = go (m + 1) h
            where
                checkLower
                    | m < 1 = Nothing
                    | k > k_1 = Just (m - 1, x_1)
                    | otherwise = Nothing
                m = (l + h) `div` 2
                x = lst V.! m
                k' = f x
                x_1 = lst V.! (m - 1)
                k_1 = f x_1

getIndex :: Int -> CompactMap k v -> Maybe v
getIndex i (CompactMap lst _) = lst V.!? i
