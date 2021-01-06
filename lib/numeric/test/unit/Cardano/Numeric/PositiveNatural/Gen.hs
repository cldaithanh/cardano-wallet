{-# LANGUAGE TypeApplications #-}

module Cardano.Numeric.PositiveNatural.Gen
    ( genPositiveNaturalAny
    , shrinkPositiveNaturalAny
    ) where

import Prelude

import Cardano.Numeric.PositiveNatural
    ( PositiveNatural )
import Data.Maybe
    ( fromMaybe, mapMaybe )
import Test.QuickCheck
    ( Arbitrary (..), Gen, Positive (..), shrink )

import qualified Cardano.Numeric.PositiveNatural as PN

--------------------------------------------------------------------------------
-- Positive natural numbers chosen from the full range available, but biased
-- toward smaller numbers.
--------------------------------------------------------------------------------

genPositiveNaturalAny :: Gen PositiveNatural
genPositiveNaturalAny
    = fromMaybe PN.one
    . PN.fromIntegral
    . getPositive @Integer <$> arbitrary

shrinkPositiveNaturalAny :: PositiveNatural -> [PositiveNatural]
shrinkPositiveNaturalAny
    = mapMaybe (PN.fromIntegral @Integer)
    . shrink
    . PN.toInteger
