{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.CoinSelection.Internal.Types.ValueMap
    where

import Prelude hiding
    ( subtract )

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Wallet.CoinSelection.Internal.Types.Difference
    ( Difference (..) )
import Cardano.Wallet.CoinSelection.Internal.Types.Equipartition
    ( Equipartition (..) )
import Cardano.Wallet.CoinSelection.Internal.Types.Partition
    ( Partition (..) )
import Cardano.Wallet.CoinSelection.Internal.Types.Subtract
    ( Subtract (..) )
import Control.Monad
    ( foldM )
import Data.Bifunctor
    ( bimap )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Set
    ( Set )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype ValueMap k v = ValueMap
    { unValueMap :: Map k v }
    deriving (Eq, Generic)
    deriving (Read, Show) via (Quiet (ValueMap k v))

newtype Keys a = Keys
    { unKeys :: a }
    deriving (Eq, Monoid, Semigroup, Show)

newtype Values a = Values
    { unValues :: a }
    deriving (Eq, Monoid, Semigroup, Show)

instance (Ord k, Difference v, Eq v, Monoid v) => Difference (ValueMap k v)
  where
    m1 `difference` m2 = F.foldl' reduce m1 (toList m2)
      where
        reduce :: ValueMap k v -> (k, v) -> ValueMap k v
        reduce m (k, v) = adjust m k (`difference` v)

instance (Ord k, Eq v, Monoid v) => Equipartition (Keys (ValueMap k v))
  where
    equipartition (Keys m) count =
        Keys . fromSequence . fmap (\k -> (k, get m k)) . F.toList <$> keySets
      where
        keySets :: NonEmpty (Set k)
        keySets = equipartition (keys m) count

    equipartitionDistance (Keys m1) (Keys m2) = equipartitionDistance
        (fromIntegral @Int @Natural $ size m1)
        (fromIntegral @Int @Natural $ size m2)

    equipartitionOrdering (Keys m1) (Keys m2) = size m1 <= size m2

instance (Ord k, Eq v, Equipartition v, Monoid v, Ord v) =>
    Equipartition (Values (ValueMap k v))
  where
    equipartition (Values m) count =
        Values <$> F.foldl' acc (mempty <$ count) (toList m)
      where
        acc :: NonEmpty (ValueMap k v) -> (k, v) -> NonEmpty (ValueMap k v)
        acc ms (k, v) = NE.zipWith (<>) ms $
            singleton k <$> equipartition v count

    equipartitionDistance (Values m1) (Values m2) =
        maybe 0 maximum (NE.nonEmpty distances)
      where
        allKeys :: Set k
        allKeys = keys m1 <> keys m2

        distances :: [Natural]
        distances = distanceForKey <$> F.toList allKeys

        distanceForKey :: k -> Natural
        distanceForKey k = get m1 k `equipartitionDistance` get m2 k

    equipartitionOrdering (Values m1) (Values m2) =
        m1 `leq` m2

instance (Ord k, Eq v, Monoid v) => Monoid (ValueMap k v)
  where
    mempty = ValueMap Map.empty

instance (Ord k, Monoid v, Ord v) => PartialOrd (ValueMap k v)
  where
    m1 `leq` m2 = F.all
        (\a -> get m1 a <= get m2 a)
        (keys m1 `Set.union` keys m2)

instance (Ord k, Eq v, Monoid v, Partition v) => Partition (ValueMap k v)
  where
    partition m xs =
        ( mconcat (fst <$> partitions)
        , F.foldl'
            (NE.zipWith (<>))
            (mempty <$ xs)
            (snd <$> partitions)
        )
      where
        partitions :: [(ValueMap k v, NonEmpty (ValueMap k v))]
        partitions = partitionForKey <$> F.toList (keys m)

        partitionForKey :: k -> (ValueMap k v, NonEmpty (ValueMap k v))
        partitionForKey k = bimap
            (singleton k)
            (fmap (singleton k))
            (partition (m `get` k) ((`get` k) <$> xs))

instance (Ord k, Eq v, Monoid v) => Semigroup (ValueMap k v)
  where
    m1 <> m2 = F.foldl' acc m1 $ toList m2
      where
        acc m (k, v) = adjust m k (<> v)

instance (Ord k, Eq v, Monoid v, Subtract v) => Subtract (ValueMap k v)
  where
    m1 `subtract` m2 = foldM acc m1 (toList m2)
      where
        acc :: ValueMap k v -> (k, v) -> Maybe (ValueMap k v)
        acc m (k, v) = adjustF m k (`subtract` v)

fromSequence :: (Foldable f, Ord k, Monoid v, Eq v) => f (k, v) -> ValueMap k v
fromSequence = F.foldl' acc mempty
  where
    acc m (k, v) = adjust m k (<> v)

singleton :: (Ord k, Eq v, Monoid v) => k -> v -> ValueMap k v
singleton = set mempty

size :: ValueMap k v -> Int
size = Map.size . unValueMap

toList :: ValueMap k v -> [(k, v)]
toList = Map.toList . unValueMap

keys :: ValueMap k v -> Set k
keys = Map.keysSet . unValueMap

adjust
    :: (Ord k, Eq v, Monoid v)
    => ValueMap k v
    -> k
    -> (v -> v)
    -> ValueMap k v
adjust m k a = set m k $ a $ get m k

adjustF
    :: (Functor f, Ord k, Eq v, Monoid v)
    => ValueMap k v
    -> k
    -> (v -> f v)
    -> f (ValueMap k v)
adjustF m k a = set m k <$> a (get m k)

delete :: Ord k => ValueMap k v -> k -> ValueMap k v
delete m k = ValueMap $ Map.delete k $ unValueMap m

get :: (Ord k, Monoid v) => ValueMap k v -> k -> v
get m k = fromMaybe mempty $ Map.lookup k $ unValueMap m

set :: (Ord k, Eq v, Monoid v) => ValueMap k v -> k -> v -> ValueMap k v
set m k v
    | v == mempty =
        ValueMap $ Map.delete k $ unValueMap m
    | otherwise =
        ValueMap $ Map.insert k v $ unValueMap m
