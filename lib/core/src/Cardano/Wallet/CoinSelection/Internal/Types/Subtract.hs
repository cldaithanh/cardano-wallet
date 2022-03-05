{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.CoinSelection.Internal.Types.Subtract
    where

import Prelude hiding
    ( subtract )

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Data.Functor
    ( (<&>) )
import Data.Maybe
    ( isNothing )
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

class Subtract a where
    subtract :: a -> a -> Maybe a

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

subtractLaws
    :: forall a. (Arbitrary a, Eq a, Monoid a, Show a, Subtract a)
    => Proxy a
    -> Laws
subtractLaws _ = Laws "Subtract"
    [ ( "Empty"
      , property (subtractLaw_empty @a))
    , ( "Self"
      , property (subtractLaw_self @a))
    ]

subtractPartialOrdLaws
    :: forall a. (Arbitrary a, PartialOrd a, Semigroup a, Show a, Subtract a)
    => Proxy a
    -> Laws
subtractPartialOrdLaws _ = Laws "Subtract/PartialOrd"
    [ ( "LessThanOrEqual"
      , property (subtractPartialOrdLaw_lessThanOrEqual @a))
    ]

subtractOrdLaws
    :: forall a. (Arbitrary a, Monoid a, Ord a, Show a, Subtract a)
    => Proxy a
    -> Laws
subtractOrdLaws _ = Laws "Subtract/Ord"
    [ ( "LessThanOrEqual"
      , property (subtractOrdLaw_lessThanOrEqual @a))
    , ( "GreaterThan"
      , property (subtractOrdLaw_greaterThan @a))
    ]

subtractLaw_empty
    :: (Eq a, Monoid a, Subtract a) => a -> Bool
subtractLaw_empty a = a `subtract` mempty == Just a

subtractLaw_self
    :: (Eq a, Monoid a, Subtract a) => a -> Bool
subtractLaw_self a = a `subtract` a == Just mempty

subtractPartialOrdLaw_lessThanOrEqual
    :: (PartialOrd a, Semigroup a, Subtract a) => a -> a -> Bool
subtractPartialOrdLaw_lessThanOrEqual a1 a2
    | a1 `leq` a2 = ((a2 `subtract` a1) <&> (<> a1)) == Just a2
    | a2 `leq` a1 = ((a1 `subtract` a2) <&> (<> a2)) == Just a1
    | otherwise = True

subtractOrdLaw_lessThanOrEqual
    :: (Ord a, Semigroup a, Subtract a) => a -> a -> Bool
subtractOrdLaw_lessThanOrEqual a1 a2
    | a1 <= a2 = ((a2 `subtract` a1) <&> (<> a1)) == Just a2
    | a2 <= a1 = ((a1 `subtract` a2) <&> (<> a2)) == Just a1
    | otherwise = True

subtractOrdLaw_greaterThan
    :: (Ord a, Subtract a) => a -> a -> Bool
subtractOrdLaw_greaterThan a1 a2
    | a1 > a2 = isNothing (a2 `subtract` a1)
    | a2 > a1 = isNothing (a1 `subtract` a2)
    | otherwise = True

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Subtract Natural where
    v1 `subtract` v2
        | v1 >= v2 = Just $ v1 - v2
        | otherwise = Nothing

instance Ord a => Subtract (Set a) where
    s1 `subtract` s2
        | remainder <> s2 == s1 = Just remainder
        | otherwise = Nothing
      where
        remainder = s1 `Set.difference` s2
