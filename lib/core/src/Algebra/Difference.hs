{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.Difference
    where

import Algebra.Lattice.Ordered
    ( Ordered (..) )
import Algebra.PartialOrd
    ( PartialOrd (..) )
import Algebra.PartialOrd.Operators
    ( PartialOrdOperators (..) )
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
import qualified Prelude

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class (Monoid a, PartialOrd a) => Difference a where
    difference :: a -> a -> a

law_Difference_Monoid_1 :: Difference a => a -> Bool
law_Difference_Monoid_1 a =
    mempty `difference` a == mempty

law_Difference_Monoid_2 :: Difference a => a -> Bool
law_Difference_Monoid_2 a =
    a `difference` mempty == a

law_Difference_Monoid_3 :: Difference a => a -> Bool
law_Difference_Monoid_3 a =
    a `difference` a == mempty

law_Difference_PartialOrd_1 :: Difference a => a -> a -> Bool
law_Difference_PartialOrd_1 a1 a2
    | a1 >= a2 = a1 >= (a1 `difference` a2)
    | a2 >= a1 = a2 >= (a2 `difference` a1)
    | otherwise = True

law_Difference_PartialOrd_2 :: Difference a => a -> a -> Bool
law_Difference_PartialOrd_2 a1 a2
    | a1 >= a2 = a1 `difference` (a1 `difference` a2) == a2
    | a2 >= a1 = a2 `difference` (a2 `difference` a1) == a1
    | otherwise = True

law_Difference_PartialOrd_Semigroup_1 :: Difference a => a -> a -> Bool
law_Difference_PartialOrd_Semigroup_1 a1 a2 =
    a1 >= ((a1 <> a2) `difference` a2)

law_Difference_PartialOrd_Semigroup_2 :: Difference a => a -> a -> Bool
law_Difference_PartialOrd_Semigroup_2 a1 a2
    | a1 >= a2 = (a1 `difference` a2) <> a2 == a1
    | a2 >= a1 = (a2 `difference` a1) <> a1 == a2
    | otherwise = True

law_Difference_PartialOrd_Monoid_1 :: Difference a => a -> a -> Bool
law_Difference_PartialOrd_Monoid_1 a1 a2
    | a1 <= a2 = a1 `difference` a2 == mempty
    | a2 <= a1 = a2 `difference` a1 == mempty
    | otherwise = True

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

differenceLaws
    :: forall a. (Arbitrary a, Show a, Difference a)
    => Proxy a
    -> Laws
differenceLaws _ = Laws "Difference"
    [ ( "Difference Monoid #1"
      , unaryProperty law_Difference_Monoid_1)
    , ( "Difference Monoid #2"
      , unaryProperty law_Difference_Monoid_2)
    , ( "Difference Monoid #3"
      , unaryProperty law_Difference_Monoid_3)
    , ( "Difference PartialOrd #1"
      , binaryProperty law_Difference_PartialOrd_1)
    , ( "Difference PartialOrd #2"
      , binaryProperty law_Difference_PartialOrd_2)
    , ( "Difference PartialOrd Semigroup #1"
      , binaryProperty law_Difference_PartialOrd_Semigroup_1)
    , ( "Difference PartialOrd Semigroup #2"
      , binaryProperty law_Difference_PartialOrd_Semigroup_2)
    , ( "Difference PartialOrd Monoid #1"
      , binaryProperty law_Difference_PartialOrd_Monoid_1)
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

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Difference (Sum Natural) where
    Sum n1 `difference` Sum n2
        | Ordered n1 >= Ordered n2 = Sum (n1 - n2)
        | otherwise = 0

instance PartialOrd (Sum Natural) where
    leq = (Prelude.<=)

instance Ord a => Difference (Set a) where
    difference = Set.difference
