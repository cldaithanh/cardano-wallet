{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.CoinSelection.Internal.Types.Difference
    where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
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

differenceLaws
    :: forall a. (Arbitrary a, Difference a, Eq a, Monoid a, Show a)
    => Proxy a
    -> Laws
differenceLaws _ = Laws "Difference"
    [ ( "Empty"
      , property (differenceLaw_empty @a))
    , ( "Self"
      , property (differenceLaw_self @a))
    ]

differencePartialOrdLaws
    :: forall a. (Arbitrary a, Difference a, PartialOrd a, Semigroup a, Show a)
    => Proxy a
    -> Laws
differencePartialOrdLaws _ = Laws "Difference/PartialOrd"
    [ ( "LessThanOrEqual"
      , property (differencePartialOrdLaw_lessThanOrEqual @a))
    ]

differenceOrdLaws
    :: forall a. (Arbitrary a, Difference a, Monoid a, Ord a, Show a)
    => Proxy a
    -> Laws
differenceOrdLaws _ = Laws "Difference/Ord"
    [ ( "LessThanOrEqual"
      , property (differenceOrdLaw_lessThanOrEqual @a))
    , ( "GreaterThan"
      , property (differenceOrdLaw_greaterThan @a))
    ]

differenceLaw_empty
    :: (Difference a, Eq a, Monoid a) => a -> Bool
differenceLaw_empty a = a `difference` mempty == a

differenceLaw_self
    :: (Difference a, Eq a, Monoid a) => a -> Bool
differenceLaw_self a = a `difference` a == mempty

differencePartialOrdLaw_lessThanOrEqual
    :: (Difference a, PartialOrd a, Semigroup a) => a -> a -> Bool
differencePartialOrdLaw_lessThanOrEqual a1 a2
    | a1 `leq` a2 = ((a2 `difference` a1) <> a1) == a2
    | a2 `leq` a1 = ((a1 `difference` a2) <> a2) == a1
    | otherwise = True

differenceOrdLaw_lessThanOrEqual
    :: (Difference a, Ord a, Semigroup a) => a -> a -> Bool
differenceOrdLaw_lessThanOrEqual a1 a2
    | a1 <= a2 = ((a2 `difference` a1) <> a1) == a2
    | a2 <= a1 = ((a1 `difference` a2) <> a2) == a1
    | otherwise = True

differenceOrdLaw_greaterThan
    :: (Difference a, Monoid a, Ord a) => a -> a -> Bool
differenceOrdLaw_greaterThan a1 a2
    | a1 > a2 = (a2 `difference` a1) == mempty
    | a2 > a1 = (a1 `difference` a2) == mempty
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
