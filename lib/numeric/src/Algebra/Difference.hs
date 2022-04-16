{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

law_Difference_Monoid_1 :: Difference a => a -> Bool
law_Difference_Monoid_1 a =
    mempty `difference` a == mempty

law_Difference_Monoid_2 :: Difference a => a -> Bool
law_Difference_Monoid_2 a =
    a `difference` mempty == a

law_Difference_Monoid_3 :: Difference a => a -> Bool
law_Difference_Monoid_3 a =
    a `difference` a == mempty

law_Difference_PartialOrd_Semigroup_1 :: Difference a => (a, a) -> Bool
law_Difference_PartialOrd_Semigroup_1 (a1, a2) =
    ((a1 <> a2) `difference` a2) <= a1

law_Difference_PartialOrd_1 :: Difference a => Ordered (a, a) -> Bool
law_Difference_PartialOrd_1 (ordered -> (a1, a2)) =
    (a2 `difference` a1) <= a2

law_Difference_PartialOrd_2 :: Difference a => Ordered (a, a) -> Bool
law_Difference_PartialOrd_2 (ordered -> (a1, a2)) =
    a2 `difference` (a2 `difference` a1) == a1

law_Difference_PartialOrd_Semigroup_2 :: Difference a => Ordered (a, a) -> Bool
law_Difference_PartialOrd_Semigroup_2 (ordered -> (a1, a2)) =
    (a2 `difference` a1) <> a1 == a2

law_Difference_PartialOrd_Monoid_1 :: Difference a => Ordered (a, a) -> Bool
law_Difference_PartialOrd_Monoid_1 (ordered -> (a1, a2)) =
    a1 `difference` a2 == mempty

law_Difference_PartialOrd_Monoid_2 :: Difference a => Ordered (a, a) -> Bool
law_Difference_PartialOrd_Monoid_2 (ordered -> (a1, a2)) =
    a2 `difference` a1 >= mempty

law_wibble :: Difference a => Ordered (a, a) -> a -> Bool
law_wibble (ordered -> (a1, a2)) a3 =
    (a1 `difference` a3) <= (a2 `difference` a3)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

differenceLaws
    :: forall a. (Arbitrary a, Show a, Difference a)
    => Proxy a
    -> Laws
differenceLaws _ = Laws "Difference"
    [ ( "Difference Monoid #1"
      , property $ law_Difference_Monoid_1 @a)
    , ( "Difference Monoid #2"
      , property $ law_Difference_Monoid_2 @a)
    , ( "Difference Monoid #3"
      , property $ law_Difference_Monoid_3 @a)
    , ( "Difference PartialOrd #1"
      , property $ law_Difference_PartialOrd_1 @a)
    , ( "Difference PartialOrd #2"
      , property $ law_Difference_PartialOrd_2 @a)
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
