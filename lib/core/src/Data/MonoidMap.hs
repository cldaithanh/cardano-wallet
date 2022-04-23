{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.MonoidMap
    (
--  * Type
      MonoidMap

--  * Modifiers
    , Keys (..)
    , Values (..)

--  * Construction
    , fromMap
    , singleton

--  * Deconstruction
    , toMap

--  * Queries
    , get
    , keys
    , size

--  * Modification
    , adjust
    , adjustF
    , delete
    , set
    )
    where

import Prelude hiding
    ( subtract )

import Algebra.Difference
    ( Difference (..) )
import Algebra.Equipartition
    ( Equipartition (..) )
import Algebra.PartialOrd
    ( PartialOrd (..) )
import Algebra.Partition
    ( Partition (..) )
import Algebra.Subtract
    ( Subtract (..) )
import Control.Monad
    ( foldM )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( on )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Monoid.Monus
    ( Monus (..), OverlappingGCDMonoid (..) )
import Data.Semigroup.Cancellative
    ( RightCancellative, LeftCancellative, Cancellative, Commutative, LeftReductive (..), Reductive (..), RightReductive (..) )
import Data.Set
    ( Set )
import GHC.Exts
    ( IsList (..) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap.Internal as Internal
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype MonoidMap k v = MonoidMap
    { unMonoidMap :: Internal.MonoidMap k v }
    deriving (Eq, Generic)
    deriving newtype (Read, Show)

--------------------------------------------------------------------------------
-- Modifiers
--------------------------------------------------------------------------------

newtype Keys a = Keys
    { unKeys :: a }
    deriving (Eq, Monoid, Semigroup, Show)

newtype Values a = Values
    { unValues :: a }
    deriving (Eq, Monoid, Semigroup, Show)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance (Ord k, Eq v, Monoid v, Commutative v) =>
    Commutative (MonoidMap k v)

-- TODO: Check the ordering of these with something that is not commutative:

instance (Ord k, Eq v, Monoid v, LeftReductive v) =>
    LeftReductive (MonoidMap k v)
  where
    isPrefixOf = isSubmapOfBy isPrefixOf
    stripPrefix = reduceWith stripPrefix

instance (Ord k, Eq v, Monoid v, RightReductive v) =>
    RightReductive (MonoidMap k v)
  where
    isSuffixOf = isSubmapOfBy isSuffixOf
    stripSuffix = reduceWith stripSuffix

instance (Ord k, Eq v, Monoid v, Reductive v) =>
    Reductive (MonoidMap k v)
  where
    (</>) = reduceWith (</>)

instance (Ord k, Eq v, Monoid v, LeftCancellative v) =>
    LeftCancellative (MonoidMap k v)

instance (Ord k, Eq v, Monoid v, RightCancellative v) =>
    RightCancellative (MonoidMap k v)

instance (Ord k, Eq v, Monoid v, Cancellative v) =>
    Cancellative (MonoidMap k v)

instance (Ord k, Eq v, Monoid v, Monus v, Reductive v) =>
    OverlappingGCDMonoid (MonoidMap k v)
  where
    overlap m1 m2 = fromList $ keyOverlap <$> F.toList (keys m1 <> keys m2)
      where
        keyOverlap :: k -> (k, v)
        keyOverlap k = (k, (m1 `get` k) `overlap` (m2 `get` k))

    stripOverlap m1 m2 = (m1 <\> m2, m1 `overlap` m2, m2 <\> m1)

-- TODO: reuse the reduceWith function here, but find a way to make reduceWith
-- work with the identity monad.

instance (Ord k, Eq v, Monoid v, Monus v, Reductive v) =>
    Monus (MonoidMap k v)
  where
    m1 <\> m2 = F.foldl' reduce m1 (toList m2)
      where
        reduce :: MonoidMap k v -> (k, v) -> MonoidMap k v
        reduce m (k, v) = adjust m k (<\> v)

instance (Ord k, Difference v, Eq v, Monoid v) =>
    Difference (MonoidMap k v)
  where
    m1 `difference` m2 = F.foldl' reduce m1 (toList m2)
      where
        reduce :: MonoidMap k v -> (k, v) -> MonoidMap k v
        reduce m (k, v) = adjust m k (`difference` v)

instance (Ord k, Eq v, Monoid v) => Equipartition (Keys (MonoidMap k v))
  where
    equipartition m = fmap (Keys . fromMap) . equipartition (toMap $ unKeys m)
    equipartitionDistance = equipartitionDistance `on` toMap . unKeys
    equipartitionOrdering = equipartitionOrdering `on` toMap . unKeys

instance (Ord k, Eq v, Equipartition v, Monoid v, PartialOrd v) =>
    Equipartition (Values (MonoidMap k v))
  where
    equipartition (Values m) count =
        Values <$> F.foldl' acc (mempty <$ count) (toList m)
      where
        acc :: NonEmpty (MonoidMap k v) -> (k, v) -> NonEmpty (MonoidMap k v)
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

instance (Ord k, Eq v, Monoid v) => IsList (MonoidMap k v)
  where
    type Item (MonoidMap k v) = (k, v)
    fromList =
        F.foldl' acc (MonoidMap Internal.empty)
      where
        acc m (k, v) = adjust m k (<> v)

    toList = Map.toList . Internal.toMap . unMonoidMap

instance (Ord k, Eq v, Monoid v) => Monoid (MonoidMap k v)
  where
    mempty = MonoidMap Internal.empty

instance (Ord k, Monoid v, PartialOrd v) =>
    PartialOrd (MonoidMap k v)
  where
    leq = isSubmapOfBy leq

instance (Ord k, Eq v, Monoid v, Partition v) => Partition (MonoidMap k v)
  where
    partition m xs =
        ( mconcat (fst <$> partitions)
        , F.foldl'
            (NE.zipWith (<>))
            (mempty <$ xs)
            (snd <$> partitions)
        )
      where
        partitions :: [(MonoidMap k v, NonEmpty (MonoidMap k v))]
        partitions = partitionForKey <$> F.toList (keys m)

        partitionForKey :: k -> (MonoidMap k v, NonEmpty (MonoidMap k v))
        partitionForKey k = bimap
            (singleton k)
            (fmap (singleton k))
            (partition (m `get` k) ((`get` k) <$> xs))

instance (Ord k, Eq v, Monoid v) => Semigroup (MonoidMap k v)
  where
    m1 <> m2 = F.foldl' acc m1 $ toList m2
      where
        acc m (k, v) = adjust m k (<> v)

instance (Ord k, Eq v, Monoid v, Subtract v) => Subtract (MonoidMap k v)
  where
    m1 `subtract` m2 = foldM acc m1 (toList m2)
      where
        acc :: MonoidMap k v -> (k, v) -> Maybe (MonoidMap k v)
        acc m (k, v) = adjustF m k (`subtract` v)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

fromMap :: (Ord k, Eq v, Monoid v) => Map k v -> MonoidMap k v
fromMap = fromList . Map.toList

singleton :: (Ord k, Eq v, Monoid v) => k -> v -> MonoidMap k v
singleton = set mempty

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

toMap :: MonoidMap k v -> Map k v
toMap = Internal.toMap . unMonoidMap

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

get :: (Ord k, Monoid v) => MonoidMap k v -> k -> v
get = Internal.get . unMonoidMap

keys :: MonoidMap k v -> Set k
keys = Map.keysSet . toMap

size :: MonoidMap k v -> Int
size = Map.size . toMap

isSubmapOfBy
    :: Ord k
    => (v1 -> v2 -> Bool)
    -> MonoidMap k v1
    -> MonoidMap k v2
    -> Bool
isSubmapOfBy f m1 m2 = Map.isSubmapOfBy f (toMap m1) (toMap m2)

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

adjust
    :: (Ord k, Eq v, Monoid v)
    => MonoidMap k v
    -> k
    -> (v -> v)
    -> MonoidMap k v
adjust m k a = set m k $ a (get m k)

adjustF
    :: (Functor f, Ord k, Eq v, Monoid v)
    => MonoidMap k v
    -> k
    -> (v -> f v)
    -> f (MonoidMap k v)
adjustF m k a = set m k <$> a (get m k)

delete :: (Ord k, Eq v, Monoid v) => MonoidMap k v -> k -> MonoidMap k v
delete m k = set m k mempty

-- TODO: this might not be reduction, but it is one-sided.
-- Find a more general name.
reduceWith
    :: (Ord k, Eq v, Monoid v)
    => (v -> v -> Maybe v)
    -> MonoidMap k v
    -> MonoidMap k v
    -> Maybe (MonoidMap k v)
reduceWith f m1 m2 =
    foldM reduce m1 (toList m2)
  where
    reduce m (k, v) = adjustF m k (f v)

set :: (Ord k, Eq v, Monoid v) => MonoidMap k v -> k -> v -> MonoidMap k v
set = ((MonoidMap .) .) . Internal.set . unMonoidMap
