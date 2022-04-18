{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Function
    ( (&) )
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

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class (Monoid a, PartialOrd a) => Difference a where
    difference :: a -> a -> a

(<\>) :: Difference a => a -> a -> a
(<\>) = difference

zero :: Difference a => a
zero = mempty

--------------------------------------------------------------------------------
-- Fundamental laws
--------------------------------------------------------------------------------

differenceLaw_d1 :: Difference a => a -> Bool
differenceLaw_d1 a =
    a <\> zero == a

differenceLaw_d2 :: Difference a => (a, a) -> Bool
differenceLaw_d2 (a, b) =
    (a <\> (b <\> a)) <\> a == zero

differenceLaw_d3 :: Difference a => (a, a) -> Bool
differenceLaw_d3 (a, b) =
    a <\> ((a <\> b) <\> (b <\> a)) == b <\> ((b <\> a) <\> (a <\> b))

differenceLaw_d4 :: Difference a => (a, a, a) -> Bool
differenceLaw_d4 (a, b, c) =
    (a <\> b) <\> (c <\> b) == (a <\> c) <\> (b <\> c)

differenceLaw_d5_1 :: Difference a => (a, a) -> Bool
differenceLaw_d5_1 (a, b) =
    a <\> (a <\> b) == (b <\> (b <\> a)) <\> ((b <\> a) <\> b)

differenceLaw_d5_2 :: Difference a => (a, a) -> Bool
differenceLaw_d5_2 (a, b) =
    (a <\> b) <\> a == ((b <\> a) <\> b) <\> (b <\> (b <\> a))

--------------------------------------------------------------------------------
-- Additional laws
--------------------------------------------------------------------------------

differenceLaw_empty_1 :: Difference a => a -> Bool
differenceLaw_empty_1 a =
    zero <\> a == zero

differenceLaw_empty_2 :: Difference a => a -> Bool
differenceLaw_empty_2 a =
    a <\> zero == a

differenceLaw_empty_3 :: Difference a => a -> Bool
differenceLaw_empty_3 a =
    a <\> a == zero

differenceLaw_inequality_1 :: Difference a => Ordered (a, a) -> Bool
differenceLaw_inequality_1 (ordered -> (a1, a2)) =
    a1 <\> a2 == zero

differenceLaw_inequality_2 :: Difference a => Ordered (a, a) -> Bool
differenceLaw_inequality_2 (ordered -> (a1, a2)) =
    a2 <\> a1 <= a2

differenceLaw_inequality_3 :: Difference a => Ordered (a, a, a) -> Bool
differenceLaw_inequality_3 (ordered -> (a1, a2, a3)) =
    (a2 <\> a1) <= (a3 <\> a1)

differenceLaw_distribution :: Difference a => Ordered (a, a, a) -> Bool
differenceLaw_distribution (ordered -> (a1, a2, a3)) =
    (a3 <\> a2) <\> a1 == a3 <\> (a2 <> a1)

differenceLaw_identity :: Difference a => Ordered (a, a) -> Bool
differenceLaw_identity (ordered -> (a1, a2)) =
    (a2 <\> a1) <> a1 == a2

differenceLaw_inversion :: Difference a => Ordered (a, a) -> Bool
differenceLaw_inversion (ordered -> (a1, a2)) =
    a2 <\> (a2 <\> a1) == a1

differenceLaw_symmetry :: Difference a => (a, a) -> Bool
differenceLaw_symmetry (a1, a2) =
    a1 <\> (a1 <\> a2) == a2 <\> (a2 <\> a1)

--------------------------------------------------------------------------------
-- Test support
--------------------------------------------------------------------------------

differenceLaws
    :: forall a. (Arbitrary a, Show a, Difference a)
    => Proxy a
    -> Laws
differenceLaws _ = Laws "Difference"
    [ makeLaw "d1"
        $ makeProperty1 differenceLaw_d1
    , makeLaw "d2"
        $ makeProperty2 differenceLaw_d2
    , makeLaw "d3"
        $ makeProperty2 differenceLaw_d3
    , makeLaw "d4"
        $ makeProperty3 differenceLaw_d4
    , makeLaw "d5_1"
        $ makeProperty2 differenceLaw_d5_1
    , makeLaw "d5_2"
        $ makeProperty2 differenceLaw_d5_2


    , makeLaw "Empty #1"
        $ makeProperty1 differenceLaw_empty_1
    , makeLaw "Empty #2"
        $ makeProperty1 differenceLaw_empty_2
    , makeLaw "Empty #3"
        $ makeProperty1 differenceLaw_empty_3
    , makeLaw "Inequality #1"
        $ makePropertyOrdered2 differenceLaw_inequality_1
    , makeLaw "Inequality #2"
        $ makePropertyOrdered2 differenceLaw_inequality_2
    , makeLaw "Inequality #3"
        $ makePropertyOrdered3 differenceLaw_inequality_3
    , makeLaw "Distribution"
        $ makePropertyOrdered3 differenceLaw_distribution
    , makeLaw "Identity"
        $ makePropertyOrdered2 differenceLaw_identity
    , makeLaw "Inversion"
        $ makePropertyOrdered2 differenceLaw_inversion
    , makeLaw "Symmetry"
        $ makeProperty2 differenceLaw_symmetry
    ]
  where
    makeLaw :: String -> Property -> (String, Property)
    makeLaw title p = (title, checkCoverage p)

    makeProperty1 :: (a -> Bool) -> Property
    makeProperty1 fn = property
        $ \a -> fn a
        & cover 1  (a == zero) "a == zero"
        & cover 50 (a /= zero) "a /= zero"

    makeProperty2 :: ((a, a) -> Bool) -> Property
    makeProperty2 fn = property
        $ \(a1, a2) -> fn (a1, a2)
        & cover 50
            (a1 /= zero && a2 /= zero)
            "a1 /= zero && a2 /= zero"

    makeProperty3 :: ((a, a, a) -> Bool) -> Property
    makeProperty3 fn = property
        $ \(a1, a2, a3) -> fn (a1, a2, a3)
        & cover 50
            (a1 /= zero && a2 /= zero && a3 /= zero)
            "a1 /= zero && a2 /= zero && a3 /= zero"

    makePropertyOrdered2 :: (Ordered (a, a) -> Bool) -> Property
    makePropertyOrdered2 fn = property
        $ \t@(ordered -> (a1, a2)) -> fn t
        & cover 50
            (a1 /= zero && a2 /= zero)
            "a1 /= zero && a2 /= zero"

    makePropertyOrdered3 :: (Ordered (a, a, a) -> Bool) -> Property
    makePropertyOrdered3 fn = property
        $ \t@(ordered -> (a1, a2, a3)) -> fn t
        & cover 50
            (a1 /= zero && a2 /= zero && a3 /= zero)
            "a1 /= zero && a2 /= zero && a3 /= zero"

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
        setToMap (mapToSet m1 <\> mapToSet m2)
      where
        mapToSet :: Map k v -> Set (k, v)
        mapToSet = Set.fromList . Map.toList

        setToMap :: Set (k, v) -> Map k v
        setToMap = Map.fromList . Set.toList
