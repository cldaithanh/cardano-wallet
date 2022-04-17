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
-- Test support
--------------------------------------------------------------------------------

differenceLaws
    :: forall a. (Arbitrary a, Show a, Difference a)
    => Proxy a
    -> Laws
differenceLaws _ = Laws "Difference"
    [ makeLaw "Empty #1"
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
        & cover 1  (a == mempty) "a == mempty"
        & cover 50 (a /= mempty) "a /= mempty"

    makeProperty2 :: ((a, a) -> Bool) -> Property
    makeProperty2 fn = property
        $ \(a1, a2) -> fn (a1, a2)
        & cover 50
            (a1 /= mempty && a2 /= mempty)
            "a1 /= mempty && a2 /= mempty"

    makePropertyOrdered2 :: (Ordered (a, a) -> Bool) -> Property
    makePropertyOrdered2 fn = property
        $ \t@(ordered -> (a1, a2)) -> fn t
        & cover 50
            (a1 /= mempty && a2 /= mempty)
            "a1 /= mempty && a2 /= mempty"

    makePropertyOrdered3 :: (Ordered (a, a, a) -> Bool) -> Property
    makePropertyOrdered3 fn = property
        $ \t@(ordered -> (a1, a2, a3)) -> fn t
        & cover 50
            (a1 /= mempty && a2 /= mempty && a3 /= mempty)
            "a1 /= mempty && a2 /= mempty && a3 /= mempty"

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
