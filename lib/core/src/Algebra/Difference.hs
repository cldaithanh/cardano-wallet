{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.Difference
    where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Data.Monoid
    ( Sum (..) )
import Data.Proxy
    ( Proxy )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary, property )
import Test.QuickCheck.Classes
    ( Laws (..) )

import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class Difference a where
    difference :: a -> a -> a

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

--              Natural
--
-- difference   | a >   b = a - b
--              | a <=  b = 0
--
-- union        | a >   b = a {1,2,3,4,5} {1,2,3,4,5,6,7}
--              | a <=  b = b
--
-- intersection | a >   b = b
--              | a <=  b = a
--
-- symmetricDifference | a >   b = a
--                     | a <=  b = b
--
-- With the above formulation, we don't have an add operation.
--
a `difference` b == a `intersection`



-- TODO: Add code coverage.
--
-- This is most important, because we need to know whether the laws and our
-- instances are actually correct.

-- TODO: Use more conventional names.
-- TODO: Make laws groups for subclasses inherit laws groups for superclasses.
-- TODO: Add sections.

law_Difference_Eq_Monoid_1
    :: (Difference a, Eq a, Monoid a) => a -> Bool
law_Difference_Eq_Monoid_1 a =
    mempty `difference` a == mempty

law_Difference_Eq_Monoid_2
    :: (Difference a, Eq a, Monoid a) => a -> Bool
law_Difference_Eq_Monoid_2 a =
    a `difference` mempty == a

law_Difference_Eq_Monoid_3
    :: (Difference a, Eq a, Monoid a) => a -> Bool
law_Difference_Eq_Monoid_3 a =
    a `difference` a == mempty

law_Difference_PartialOrd_1
    :: (Difference a, PartialOrd a) => a -> a -> Bool
law_Difference_PartialOrd_1 a1 a2
    | a1 `leq` a2 = (a2 `difference` a1) `leq` a2
    | a2 `leq` a1 = (a1 `difference` a2) `leq` a1
    | otherwise = True

law_Difference_PartialOrd_Semigroup_1
    :: (Difference a, PartialOrd a, Semigroup a) => a -> a -> Bool
law_Difference_PartialOrd_Semigroup_1 a1 a2 =
    ((a1 <> a2) `difference` a2) `leq` a1

law_Difference_PartialOrd_Semigroup_2
    :: (Difference a, PartialOrd a, Semigroup a) => a -> a -> Bool
law_Difference_PartialOrd_Semigroup_2 a1 a2
    | a1 `leq` a2 = (a2 `difference` a1) <> a1 == a2
    | a2 `leq` a1 = (a1 `difference` a2) <> a2 == a1
    | otherwise = True

law_Difference_PartialOrd_Monoid_1
    :: (Difference a, PartialOrd a, Monoid a) => a -> a -> Bool
law_Difference_PartialOrd_Monoid_1 a1 a2
    | a1 `leq` a2 = a1 `difference` a2 == mempty
    | a2 `leq` a1 = a2 `difference` a1 == mempty
    | otherwise = True

law_Difference_Ord_1
    :: (Difference a, Ord a) => a -> a -> Bool
law_Difference_Ord_1 a1 a2
    | a1 <= a2 = (a2 `difference` a1) <= a2
    | a2 <= a1 = (a1 `difference` a2) <= a1
    | otherwise = True

law_Difference_Ord_Semigroup_1
    :: (Difference a, Ord a, Semigroup a) => a -> a -> Bool
law_Difference_Ord_Semigroup_1 a1 a2 =
    ((a1 <> a2) `difference` a2) == a1

law_Difference_Ord_Semigroup_2
    :: (Difference a, Ord a, Semigroup a) => a -> a -> Bool
law_Difference_Ord_Semigroup_2 a1 a2
    | a1 <= a2 = (a2 `difference` a1) <> a1 == a2
    | a2 <= a1 = (a1 `difference` a2) <> a2 == a1
    | otherwise = True

law_Difference_Ord_Monoid_1
    :: (Difference a, Ord a, Monoid a) => a -> a -> Bool
law_Difference_Ord_Monoid_1 a1 a2
    | a1 <= a2 = a1 `difference` a2 == mempty
    | a2 <= a1 = a2 `difference` a1 == mempty
    | otherwise = True

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Difference Natural where
    n1 `difference` n2
        | n1 >= n2 = n1 - n2
        | otherwise = 0

instance Ord a => Difference (Set a) where
    difference = Set.difference

deriving instance Difference a => Difference (Sum a)

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

laws_Difference_Eq_Monoid
    :: forall a. (Arbitrary a, Show a, Difference a, Eq a, Monoid a)
    => Proxy a
    -> Laws
laws_Difference_Eq_Monoid _ = Laws "Difference Eq Monoid"
    [ ( "Difference Eq Monoid #1"
      , property (law_Difference_Eq_Monoid_1 @a))
    , ( "Difference Eq Monoid #2"
      , property (law_Difference_Eq_Monoid_2 @a))
    , ( "Difference Eq Monoid #3"
      , property (law_Difference_Eq_Monoid_3 @a))
    ]

laws_Difference_PartialOrd
    :: forall a. (Arbitrary a, Show a, Difference a, PartialOrd a)
    => Proxy a
    -> Laws
laws_Difference_PartialOrd _ = Laws "Difference PartialOrd"
    [ ( "Difference PartialOrd #1"
      , property (law_Difference_PartialOrd_1 @a))
    ]

laws_Difference_PartialOrd_Semigroup
    :: forall a. (Arbitrary a, Show a, Difference a, PartialOrd a, Semigroup a)
    => Proxy a
    -> Laws
laws_Difference_PartialOrd_Semigroup _ = Laws "Difference PartialOrd Semigroup"
    [ ( "Difference PartialOrd Semigroup #1"
      , property (law_Difference_PartialOrd_Semigroup_1 @a))
    , ( "Difference PartialOrd Semigroup #2"
      , property (law_Difference_PartialOrd_Semigroup_2 @a))
    ]

laws_Difference_PartialOrd_Monoid
    :: forall a. (Arbitrary a, Show a, Difference a, PartialOrd a, Monoid a)
    => Proxy a
    -> Laws
laws_Difference_PartialOrd_Monoid _ = Laws "Difference PartialOrd Monoid"
    [ ( "Difference PartialOrd Monoid #1"
      , property (law_Difference_PartialOrd_Monoid_1 @a))
    ]

laws_Difference_Ord
    :: forall a. (Arbitrary a, Show a, Difference a, Ord a)
    => Proxy a
    -> Laws
laws_Difference_Ord _ = Laws "Difference Ord"
    [ ( "Difference Ord #1"
      , property (law_Difference_Ord_1 @a))
    ]

laws_Difference_Ord_Semigroup
    :: forall a. (Arbitrary a, Show a, Difference a, Ord a, Semigroup a)
    => Proxy a
    -> Laws
laws_Difference_Ord_Semigroup _ = Laws "Difference Ord Semigroup"
    [ ( "Difference Ord Semigroup #1"
      , property (law_Difference_Ord_Semigroup_1 @a))
    , ( "Difference Ord Semigroup #2"
      , property (law_Difference_Ord_Semigroup_2 @a))
    ]

laws_Difference_Ord_Monoid
    :: forall a. (Arbitrary a, Show a, Difference a, Ord a, Monoid a)
    => Proxy a
    -> Laws
laws_Difference_Ord_Monoid _ = Laws "Difference Ord Monoid"
    [ ( "Difference Ord Monoid #1"
      , property (law_Difference_Ord_Monoid_1 @a))
    ]
