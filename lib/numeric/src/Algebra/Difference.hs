{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.Difference
    where

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Algebra.PartialOrd.Instances
    ()
import Algebra.PartialOrd.Operators
    ( PartialOrdOperators (..) )
import Algebra.PartialOrd.Ordered
    ( Ordered, ordered )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Sum (..) )
import Data.Proxy
    ( Proxy )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Prelude hiding
    ( Ord (..) )
import Prelude
    ( Ord )
import Test.QuickCheck
    ( Arbitrary, Property, checkCoverage, cover, property )
import Test.QuickCheck.Classes
    ( Laws (..) )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- TODO:
--
-- Express pre-conditions in laws
-- Add nullary property (mempty `difference` mempty == mempty)
-- Add property:
--    a >= b => (a `difference` c) >= (b `difference` c)

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class (Monoid a, PartialOrd a) => Difference a where
    difference :: a -> a -> a

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

differenceLaw_empty_1 :: Difference a => a -> Bool
differenceLaw_empty_1 a =
    mempty `difference` a == mempty

differenceLaw_empty_2 :: Difference a => a -> Bool
differenceLaw_empty_2 a =
    a `difference` mempty == a

differenceLaw_empty_3 :: Difference a => a -> Bool
differenceLaw_empty_3 a =
    a `difference` a == mempty

differenceLaw_inequality_1 :: Difference a => Ordered (a, a) -> Bool
differenceLaw_inequality_1 (ordered -> (a1, a2)) =
    a1 `difference` a2 == mempty

differenceLaw_inequality_2 :: Difference a => Ordered (a, a) -> Bool
differenceLaw_inequality_2 (ordered -> (a1, a2)) =
    a2 `difference` a1 <= a2

differenceLaw_inequality_3 :: Difference a => Ordered (a, a, a) -> Bool
differenceLaw_inequality_3 (ordered -> (a1, a2, a3)) =
    (a2 `difference` a1) <= (a3 `difference` a1)

differenceLaw_distribution :: Difference a => Ordered (a, a, a) -> Bool
differenceLaw_distribution (ordered -> (a1, a2, a3)) =
    (a3 `difference` a2) `difference` a1 == a3 `difference` (a2 <> a1)

differenceLaw_identity :: Difference a => Ordered (a, a) -> Bool
differenceLaw_identity (ordered -> (a1, a2)) =
    (a2 `difference` a1) <> a1 == a2

differenceLaw_inversion :: Difference a => Ordered (a, a) -> Bool
differenceLaw_inversion (ordered -> (a1, a2)) =
    a2 `difference` (a2 `difference` a1) == a1

differenceLaw_symmetry :: Difference a => (a, a) -> Bool
differenceLaw_symmetry (a1, a2) =
    a1 `difference` (a1 `difference` a2) == a2 `difference` (a2 `difference` a1)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

differenceLaws
    :: forall a. (Arbitrary a, Show a, Difference a)
    => Proxy a
    -> Laws
differenceLaws _ = Laws "Difference"
    [ makeLaw "Empty #1"
        (property $ differenceLaw_empty_1 @a)
    , makeLaw "Empty #2"
        (property $ differenceLaw_empty_2 @a)
    , makeLaw "Empty #3"
        (property $ differenceLaw_empty_3 @a)
    , makeLaw "Inequality #1"
        (property $ differenceLaw_inequality_1 @a)
    , makeLaw "Inequality #2"
        (property $ differenceLaw_inequality_2 @a)
    , makeLaw "Inequality #3"
        (property $ differenceLaw_inequality_3 @a)
    , makeLaw "Distribution"
        (property $ differenceLaw_distribution @a)
    , makeLaw "Identity"
        (property $ differenceLaw_identity @a)
    , makeLaw "Inversion"
        (property $ differenceLaw_inversion @a)
    , makeLaw "Symmetry"
        (property $ differenceLaw_symmetry @a)
    ]
  where
    makeLaw title law = (title, law)

    unaryProperty :: (a -> Bool) -> Property
    unaryProperty fn = property
        $ \a -> checkCoverage
        $ cover 1  (a == mempty) "a == mempty"
        $ cover 50 (a /= mempty) "a /= mempty"
        $ fn a
    binaryProperty :: (a -> a -> Bool) -> Property
    binaryProperty fn = property
        $ \a1 a2 -> checkCoverage
        $ cover 2 (a1 <  a2) "a1 < a2"
        $ cover 1 (a1 == a2) "a1 = a2"
        $ cover 2 (a1 >  a2) "a1 > a2"
        $ cover 0.2 (a1 > a2 && a2 > mempty) "a1 > a2 && a2 > mempty"
        $ cover 0.2 (a2 > a1 && a1 > mempty) "a2 > a1 && a1 > mempty"
        $ fn a1 a2
    ternaryProperty :: (a -> a -> a -> Bool) -> Property
    ternaryProperty fn = property
        $ \a1 a2 a3 -> checkCoverage
        $ fn a1 a2 a3

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Difference (Sum Natural) where
    Sum n1 `difference` Sum n2
        | n1 >= n2 = Sum (n1 - n2)
        | otherwise = 0

instance Ord a => Difference (Set a) where
    difference = Set.difference

newtype SetDifference a = SetDifference a
    deriving (Eq, Semigroup, Monoid)

instance (Ord k, Eq v) => PartialOrd (SetDifference (Map k v)) where
    SetDifference m1 `leq` SetDifference m2 =
        m1 `Map.isSubmapOf` m2

instance (Ord k, Ord v) => Difference (SetDifference (Map k v)) where
    SetDifference m1 `difference` SetDifference m2 = SetDifference $
        setToMap (mapToSet m1 `difference` mapToSet m2)
      where
        mapToSet :: Map k v -> Set (k, v)
        mapToSet = Set.fromList . Map.toList

        setToMap :: Set (k, v) -> Map k v
        setToMap = Map.fromList . Set.toList
