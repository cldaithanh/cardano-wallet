{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Numeric.UtilSpec
    ( spec
    ) where

import Prelude

import Cardano.Numeric.Util
    ( equipartitionNatural
    , padCoalesce
    , partitionNatural
    , zeroSmallestToFitMaxBound
    )
import Data.List
    ( isSuffixOf )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes )
import Data.Monoid
    ( Sum (..) )
import Data.Ratio
    ( (%) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitrarySizedNatural
    , checkCoverage
    , cover
    , property
    , shrink
    , shrinkIntegral
    , withMaxSuccess
    , (.&&.)
    , (.||.)
    , (===)
    )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do

    describe "padCoalesce" $ do

        it "prop_padCoalesce_length" $
            property $ prop_padCoalesce_length @(Sum Int)
        it "prop_padCoalesce_sort" $
            property $ prop_padCoalesce_sort @(Sum Int)
        it "prop_padCoalesce_sum" $
            property $ prop_padCoalesce_sum @(Sum Int)

    describe "equipartitionNatural" $ do

        it "prop_equipartitionNatural_fair" $
            property prop_equipartitionNatural_fair
        it "prop_equipartitionNatural_length" $
            property prop_equipartitionNatural_length
        it "prop_equipartitionNatural_order" $
            property prop_equipartitionNatural_order
        it "prop_equipartitionNatural_sum" $
            property prop_equipartitionNatural_sum

    describe "partitionNatural" $ do

        it "prop_partitionNatural_length" $
            property prop_partitionNatural_length
        it "prop_partitionNatural_sum" $
            property prop_partitionNatural_sum
        it "prop_partitionNatural_fair" $
            withMaxSuccess 1000 $ checkCoverage prop_partitionNatural_fair

    describe "zeroSmallestToFitMaxBound " $ do

        it "prop_zeroSmallestToFitMaxBound_coverage" $
            property prop_zeroSmallestToFitMaxBound_coverage
        it "prop_zeroSmallestToFitMaxBound_equality" $
            property prop_zeroSmallestToFitMaxBound_equality
        it "prop_zeroSmallestToFitMaxBound_length" $
            property prop_zeroSmallestToFitMaxBound_length
        it "prop_zeroSmallestToFitMaxBound_suffix" $
            property prop_zeroSmallestToFitMaxBound_suffix
        it "prop_zeroSmallestToFitMaxBound_sum" $
            property prop_zeroSmallestToFitMaxBound_sum

--------------------------------------------------------------------------------
-- Coalescing values
--------------------------------------------------------------------------------

prop_padCoalesce_length
    :: (Monoid a, Ord a, Show a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_length source target =
    NE.length (padCoalesce source target) === NE.length target

prop_padCoalesce_sort
    :: (Monoid a, Ord a, Show a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_sort source target =
    NE.sort result === result
  where
    result = padCoalesce source target

prop_padCoalesce_sum
    :: (Monoid a, Ord a, Show a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_sum source target =
    F.fold source === F.fold (padCoalesce source target)

--------------------------------------------------------------------------------
-- Equipartitioning natural numbers
--------------------------------------------------------------------------------

-- Test that natural numbers are equipartitioned fairly:
--
-- Each portion must be within unity of the ideal portion.
--
prop_equipartitionNatural_fair
    :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_fair n count = (.||.)
    (difference === 0)
    (difference === 1)
  where
    difference :: Natural
    difference = F.maximum results - F.minimum results

    results :: NonEmpty Natural
    results = equipartitionNatural n count

prop_equipartitionNatural_length :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_length n count =
    NE.length (equipartitionNatural n count) === NE.length count

prop_equipartitionNatural_order :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_order n count =
    NE.sort results === results
  where
    results = equipartitionNatural n count

prop_equipartitionNatural_sum :: Natural -> NonEmpty () -> Property
prop_equipartitionNatural_sum n count =
    F.sum (equipartitionNatural n count) === n

--------------------------------------------------------------------------------
-- Partitioning natural numbers
--------------------------------------------------------------------------------

prop_partitionNatural_length
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_partitionNatural_length target weights =
    case partitionNatural target weights of
        Nothing -> F.sum weights === 0
        Just ps -> F.length ps === F.length weights

prop_partitionNatural_sum
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_partitionNatural_sum target weights =
    case partitionNatural target weights of
        Nothing -> F.sum weights === 0
        Just ps -> F.sum ps === target

-- | Check that portions are all within unity of ideal unrounded portions.
--
prop_partitionNatural_fair
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_partitionNatural_fair target weights =
    case partitionNatural target weights of
        Nothing -> F.sum weights === 0
        Just ps -> prop ps
  where
    prop portions = (.&&.)
        (F.all (uncurry (<=)) (NE.zip portions portionUpperBounds))
        (F.all (uncurry (>=)) (NE.zip portions portionLowerBounds))
      where
        portionUpperBounds = ceiling . computeIdealPortion <$> weights
        portionLowerBounds = floor   . computeIdealPortion <$> weights

        computeIdealPortion :: Natural -> Rational
        computeIdealPortion c
            = fromIntegral target
            * fromIntegral c
            % fromIntegral totalWeight

        totalWeight :: Natural
        totalWeight = F.sum weights

--------------------------------------------------------------------------------
-- Zeroing out small values to fit a maximum upper bound
--------------------------------------------------------------------------------

-- TODO: Use a dedicated data type to get the coverage we want.
--
-- It should test with
--
-- - different lengths of lists.
-- - arrange that we cover:
--     - one of the items being zeroed out
--     - all of the items being zeroed out
--     - other proportions
--
prop_zeroSmallestToFitMaxBound_coverage
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_zeroSmallestToFitMaxBound_coverage upperBound as =
    property $
    checkCoverage $
    cover 10 (asSum > upperBound)
        "asSum > upperBound" $
    cover 1 (asSum == upperBound)
        "asSum = upperBound" $
    cover 1 (asSum < upperBound)
        "asSum < upperBound" $
    cover 10 (rsSum > 0)
        "rsSum > 0" $
    cover 10 (rsNonZeroCount > 1)
        "rsNonZeroCount > 1" $
    True
  where
    asSum = F.sum as
    rsSum = F.sum rs
    rsNonZeroCount = length $ filter (> 0) $ F.toList rs
    rs = zeroSmallestToFitMaxBound upperBound as

prop_zeroSmallestToFitMaxBound_equality
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_zeroSmallestToFitMaxBound_equality upperBound as
    | total <= upperBound =
        as === rs
    | otherwise =
        property $ F.all (\(r, a) -> r == a || r == 0) (rs `NE.zip` as)
  where
    rs = zeroSmallestToFitMaxBound upperBound as
    total = F.sum as

prop_zeroSmallestToFitMaxBound_length
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_zeroSmallestToFitMaxBound_length upperBound as =
    NE.length as === NE.length (zeroSmallestToFitMaxBound upperBound as)

prop_zeroSmallestToFitMaxBound_suffix
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_zeroSmallestToFitMaxBound_suffix upperBound as =
    property $ dropWhile (== 0) rsSorted `isSuffixOf` asSorted
  where
    asSorted = NE.toList $ NE.sort as
    rsSorted = NE.toList $ NE.sort $ zeroSmallestToFitMaxBound upperBound as

prop_zeroSmallestToFitMaxBound_sum
    :: Natural
    -> NonEmpty Natural
    -> Property
prop_zeroSmallestToFitMaxBound_sum upperBound as =
    property $ F.sum (zeroSmallestToFitMaxBound upperBound as) <= upperBound

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink xs = catMaybes $ NE.nonEmpty <$> shrink (NE.toList xs)

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral
