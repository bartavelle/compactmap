module Data.CompactMap
    ( CompactMap
    , fromList
    , toVector
    , lookup
    , getLE
    , getIndex
    ) where

import qualified Data.CompactMap.Generic as CM
import qualified Data.Vector as V
import Data.Foldable as F
import Prelude hiding (lookup)

newtype CompactMap k v = CompactMap { getCM :: CM.CompactMap V.Vector k v }

instance Show v => Show (CompactMap k v) where
    show = show . V.toList . getMap

instance Foldable (CompactMap k) where
    foldr f i = F.foldr f i . getMap
    foldMap f = F.foldMap f . getMap

getMap :: CompactMap k v -> V.Vector v
getMap = CM.toVector . getCM
{-# INLINE getMap #-}

fromList :: Ord k => [v] -> (v -> k) -> CompactMap k v
fromList lst f = CompactMap (CM.fromList lst f)

toVector :: CompactMap k v -> V.Vector v
toVector = getMap
{-# INLINE toVector #-}

lookup :: Ord k => k -> CompactMap k v -> Maybe v
lookup k (CompactMap cm) = CM.lookup k cm

getLE :: Ord k => k -> CompactMap k v -> Maybe (Int, v)
getLE k (CompactMap cm) = CM.getLE k cm

getIndex :: Int -> CompactMap k v -> Maybe v
getIndex i (CompactMap cm) = CM.getIndex i cm
