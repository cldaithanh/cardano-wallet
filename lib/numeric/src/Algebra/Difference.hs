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

differenceLaw_1 :: Difference a => a -> Bool
differenceLaw_1 a =
    mempty `difference` a == mempty

differenceLaw_2 :: Difference a => a -> Bool
differenceLaw_2 a =
    a `difference` mempty == a

differenceLaw_3 :: Difference a => a -> Bool
differenceLaw_3 a =
    a `difference` a == mempty

differenceLaw_10 :: Difference a => (a, a) -> Bool
differenceLaw_10 (a1, a2) =
    a1 `difference` (a1 `difference` a2) == a2 `difference` (a2 `difference` a1)

differenceLaw_4 :: Difference a => Ordered (a, a) -> Bool
differenceLaw_4 (ordered -> (a1, a2)) =
    a1 `difference` a2 == mempty

differenceLaw_8 :: Difference a => Ordered (a, a) -> Bool
differenceLaw_8 (ordered -> (a1, a2)) =
    a2 `difference` a1 <= a2

differenceLaw_9 :: Difference a => Ordered (a, a) -> Bool
differenceLaw_9 (ordered -> (a1, a2)) =
    (a2 `difference` a1) <> a1 == a2

differenceLaw_6 :: Difference a => Ordered (a, a) -> Bool
differenceLaw_6 (ordered -> (a1, a2)) =
    a2 `difference` (a2 `difference` a1) == a1

differenceLaw_7 :: Difference a => Ordered (a, a, a) -> Bool
differenceLaw_7 (ordered -> (a1, a2, a3)) =
    (a2 `difference` a1) <= (a3 `difference` a1)

differenceLaw_5 :: Difference a => Ordered (a, a, a) -> Bool
differenceLaw_5 (ordered -> (a1, a2, a3)) =
    (a3 `difference` a2) `difference` a1 == a3 `difference` (a2 <> a1)



--
--

law_wibble :: Difference a => Ordered (a, a) -> a -> Bool
law_wibble (ordered -> (a1, a2)) a3 =
    (a1 `difference` a3) <= (a2 `difference` a3)

differenceLaw_inverse :: Difference a => Ordered (a, a) -> Bool
differenceLaw_inverse (ordered -> (lo, hi)) =
    hi `difference` (hi `difference` lo) == lo

law_Difference_PartialOrd_Semigroup_1 :: Difference a => (a, a) -> Bool
law_Difference_PartialOrd_Semigroup_1 (a1, a2) =
    ((a1 <> a2) `difference` a2) <= a1

law_Difference_PartialOrd_1 :: Difference a => Ordered (a, a) -> Bool
law_Difference_PartialOrd_1 (ordered -> (a1, a2)) =
    (a2 `difference` a1) <= a2



law_Difference_PartialOrd_Semigroup_2 :: Difference a => Ordered (a, a) -> Bool
law_Difference_PartialOrd_Semigroup_2 (ordered -> (a1, a2)) =
    (a2 `difference` a1) <> a1 == a2

law_Difference_PartialOrd_Monoid_1 :: Difference a => Ordered (a, a) -> Bool
law_Difference_PartialOrd_Monoid_1 (ordered -> (a1, a2)) =
    a1 `difference` a2 == mempty

law_Difference_PartialOrd_Monoid_2 :: Difference a => Ordered (a, a) -> Bool
law_Difference_PartialOrd_Monoid_2 (ordered -> (a1, a2)) =
    a2 `difference` a1 >= mempty



--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

differenceLaws
    :: forall a. (Arbitrary a, Show a, Difference a)
    => Proxy a
    -> Laws
differenceLaws _ = Laws "Difference"
    [ ( "#1"
      , property $ differenceLaw_1 @a)
    , ( "#2"
      , property $ differenceLaw_2 @a)
    , ( "#3"
      , property $ differenceLaw_3 @a)
    , ( "#4"
      , property $ differenceLaw_4 @a)
    , ( "#5"
      , property $ differenceLaw_5 @a)
    , ( "#6"
      , property $ differenceLaw_6 @a)
    , ( "#7"
      , property $ differenceLaw_7 @a)
    , ( "#8"
      , property $ differenceLaw_8 @a)
    , ( "#9"
      , property $ differenceLaw_9 @a)
    , ( "#10"
      , property $ differenceLaw_10 @a)
    , ( "Difference PartialOrd #1"
      , property $ law_Difference_PartialOrd_1 @a)
    , ( "Inverse"
      , property $ differenceLaw_inverse @a)
    , ( "Inverse #2"
      , property $ differenceLaw_6 @a)
    , ( "Difference PartialOrd Semigroup #1"
      , property $ law_Difference_PartialOrd_Semigroup_1 @a)
    , ( "Difference PartialOrd Semigroup #2"
      , property $ law_Difference_PartialOrd_Semigroup_2 @a)
    , ( "Difference PartialOrd Monoid #1"
      , property $ law_Difference_PartialOrd_Monoid_1 @a)
    , ( "Difference PartialOrd Monoid #2"
      , property $ law_Difference_PartialOrd_Monoid_2 @a)
    , ( "Wibble"
      , property $ law_wibble @a)
    ]
  where
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
