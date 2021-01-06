{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Numeric.PositiveNatural
    (
    -- * Type
      PositiveNatural

    -- * Conversions
    , fromIntegral
    , toInteger
    , toNatural

    -- * Constants
    , one

    -- * Arithmetic Operations

    -- ** Unary Arithmetic Operations
    , pred
    , succ

    -- ** Binary Arithmetic Operations
    , add
    , sub
    , mul
    , div
    , mod
    , gcd
    , lcm
    , distance

    -- ** Transformations
    , Product (..)
    , Sum (..)

    ) where

import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import Prelude hiding
    ( and, div, fromIntegral, gcd, lcm, mod, or, pred, succ, toInteger )

import qualified Prelude

-- | Represents a strictly positive natural number, a member of the set
--   \( \mathbb {N}_{1} \).
--
-- Values of this type are greater than or equal to 'one'.
--
newtype PositiveNatural = PositiveNatural
    { unPositiveNatural :: Natural }
    deriving stock (Eq, Generic, Ord)
    deriving newtype Show

--------------------------------------------------------------------------------
-- Internal Functions
--------------------------------------------------------------------------------

wrap :: Natural -> PositiveNatural
wrap = PositiveNatural

unwrap :: PositiveNatural -> Natural
unwrap = unPositiveNatural

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

-- | Constructs a 'PositiveNatural' from any 'Integral' value.
--
-- Returns 'Nothing' if the specified value is zero or negative.
--
fromIntegral :: Integral i => i -> Maybe PositiveNatural
fromIntegral i
    | i > 0 =
        Just $ wrap $ Prelude.fromIntegral i
    | otherwise =
        Nothing

-- | Converts a 'PositiveNatural' to a value of type 'Integer'.
--
toInteger :: PositiveNatural -> Integer
toInteger = Prelude.fromIntegral . unwrap

-- | Converts a 'PositiveNatural' to a value of type 'Natural'.
--
toNatural :: PositiveNatural -> Natural
toNatural = unwrap

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | The smallest possible positive natural value.
--
one :: PositiveNatural
one = wrap 1

--------------------------------------------------------------------------------
-- Unary Arithmetic Operations
--------------------------------------------------------------------------------

-- | Calculates the predecessor of the given value by subtracting 'one'.
--
-- Since the result may be zero, the result is of type 'Natural'.
--
pred :: PositiveNatural -> Natural
pred = Prelude.pred . unwrap

-- | Calculates the successor of the given value by adding 'one'.
--
succ :: PositiveNatural -> PositiveNatural
succ = wrap . Prelude.succ . unwrap

--------------------------------------------------------------------------------
-- Binary Arithmetic Operations
--------------------------------------------------------------------------------

-- | Calculates the sum of two 'PositiveNatural' values.
--
add :: PositiveNatural -> PositiveNatural -> PositiveNatural
add a b = wrap $ unwrap a + unwrap b

-- | Calculates the product of two 'PositiveNatural' values.
--
mul :: PositiveNatural -> PositiveNatural -> PositiveNatural
mul a b = wrap $ unwrap a * unwrap b

-- | Subtracts the second 'PositiveNatural' value from the first.
--
-- Since the result may be negative, the result is of type 'Integer'.
--
sub :: PositiveNatural -> PositiveNatural -> Integer
sub a b = toInteger a - toInteger b

-- | Divides the first 'PositiveNatural' by the second.
--
-- The result is truncated toward zero.
--
-- Since the result may be zero, the result is of type 'Natural'.
--
div :: PositiveNatural -> PositiveNatural -> Natural
div a b = unwrap a `Prelude.div` unwrap b

-- | Finds the remainder of dividing the first 'PositiveNatural' by the second.
--
-- Since the result may be zero, the result is of type 'Natural'.
--
mod :: PositiveNatural -> PositiveNatural -> Natural
mod a b = unwrap a `Prelude.mod` unwrap b

-- | Calculates the greatest common divisor of two 'PositiveNatural' values.
--
gcd :: PositiveNatural -> PositiveNatural -> PositiveNatural
gcd a b = wrap $ unwrap a `Prelude.gcd` unwrap b

-- | Calculates the lowest common multiple of two 'PositiveNatural' values.
--
lcm :: PositiveNatural -> PositiveNatural -> PositiveNatural
lcm a b = wrap $ unwrap a `Prelude.lcm` unwrap b

-- | Finds the absolute difference between two 'PositiveNatural' values.
--
-- Since the distance may be zero, the result is of type 'Natural'.
--
distance :: PositiveNatural -> PositiveNatural -> Natural
distance a b
    | a > b = unwrap a - unwrap b
    | b > a = unwrap b - unwrap a
    | otherwise = 0

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

newtype Product = Product { unProduct :: PositiveNatural }

instance Semigroup Product where
    Product a <> Product b = Product $ mul a b

instance Monoid Product where
    mempty = Product one

newtype Sum = Sum { unSum :: PositiveNatural }

instance Semigroup Sum where
    Sum a <> Sum b = Sum $ add a b
