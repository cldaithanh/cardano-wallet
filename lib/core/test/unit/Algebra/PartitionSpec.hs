{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.PartitionSpec
    where

import Prelude

import Algebra.Partition
    ( Partition (..), partitionLaws )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Monoid
    ( Sum (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), arbitrarySizedIntegral )
import Test.Hspec.Unit
    ( UnitTestData2
    , unitTestData2
    , unitTestSpec
    )
import Test.Utils.Laws
    ( testLawsMany )

spec :: Spec
spec = do
    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(Sum Natural)
            [ partitionLaws
            ]

    parallel $ describe "partition" $ do

        describe "unit tests" $ do
            unitTestSpec_partition_Natural

--------------------------------------------------------------------------------
-- Unit tests: Partition Natural
--------------------------------------------------------------------------------

unitTestSpec_partition_Natural :: Spec
unitTestSpec_partition_Natural = unitTestSpec
    "partition Natural"
    "partition"
    (partition)
    (unitTestData_partition_Natural)

unitTestData_partition_Natural :: UnitTestData2
    (Natural)
    (NonEmpty Natural)
    (Natural, NonEmpty Natural)
unitTestData_partition_Natural = unitTestData2
    [ (  1, [              0], ( 1, [              0]) )
    , (  1, [          0,  0], ( 1, [          0,  0]) )
    , (  1, [      0,  0,  0], ( 1, [      0,  0,  0]) )
    , (  1, [  0,  0,  0,  0], ( 1, [  0,  0,  0,  0]) )

    , ( 99, [              0], (99, [              0]) )
    , ( 99, [          0,  0], (99, [          0,  0]) )
    , ( 99, [      0,  0,  0], (99, [      0,  0,  0]) )
    , ( 99, [  0,  0,  0,  0], (99, [  0,  0,  0,  0]) )

    , (  1, [              1], ( 0, [              1]) )
    , (  1, [          0,  1], ( 0, [          0,  1]) )
    , (  1, [      0,  0,  1], ( 0, [      0,  0,  1]) )
    , (  1, [  0,  0,  0,  1], ( 0, [  0,  0,  0,  1]) )

    , (  1, [              1], ( 0, [              1]) )
    , (  1, [          1,  0], ( 0, [          1,  0]) )
    , (  1, [      1,  0,  0], ( 0, [      1,  0,  0]) )
    , (  1, [  1,  0,  0,  0], ( 0, [  1,  0,  0,  0]) )

    , (  1, [              1], ( 0, [              1]) )
    , (  1, [          1,  1], ( 0, [          0,  1]) )
    , (  1, [      1,  1,  1], ( 0, [      0,  0,  1]) )
    , (  1, [  1,  1,  1,  1], ( 0, [  0,  0,  0,  1]) )

    , (  2, [              1], ( 0, [              2]) )
    , (  2, [          1,  1], ( 0, [          1,  1]) )
    , (  2, [      1,  1,  1], ( 0, [      0,  1,  1]) )
    , (  2, [  1,  1,  1,  1], ( 0, [  0,  0,  1,  1]) )

    , (  3, [              1], ( 0, [              3]) )
    , (  3, [          1,  1], ( 0, [          1,  2]) )
    , (  3, [      1,  1,  1], ( 0, [      1,  1,  1]) )
    , (  3, [  1,  1,  1,  1], ( 0, [  0,  1,  1,  1]) )

    , (  4, [              1], ( 0, [              4]) )
    , (  4, [          1,  1], ( 0, [          2,  2]) )
    , (  4, [      1,  1,  1], ( 0, [      1,  1,  2]) )
    , (  4, [  1,  1,  1,  1], ( 0, [  1,  1,  1,  1]) )

    , (  0, [  1,  2,  4,  8], ( 0, [  0,  0,  0,  0]) )
    , (  1, [  1,  2,  4,  8], ( 0, [  0,  0,  0,  1]) )
    , (  2, [  1,  2,  4,  8], ( 0, [  0,  0,  0,  2]) )
    , (  3, [  1,  2,  4,  8], ( 0, [  0,  0,  1,  2]) )
    , (  4, [  1,  2,  4,  8], ( 0, [  0,  0,  1,  3]) )
    , (  5, [  1,  2,  4,  8], ( 0, [  0,  0,  2,  3]) )
    , (  6, [  1,  2,  4,  8], ( 0, [  0,  0,  2,  4]) )
    , (  7, [  1,  2,  4,  8], ( 0, [  0,  1,  2,  4]) )
    , (  8, [  1,  2,  4,  8], ( 0, [  0,  1,  2,  5]) )
    , (  9, [  1,  2,  4,  8], ( 0, [  0,  1,  3,  5]) )
    , ( 10, [  1,  2,  4,  8], ( 0, [  0,  1,  3,  6]) )
    , ( 11, [  1,  2,  4,  8], ( 0, [  0,  2,  3,  6]) )
    , ( 12, [  1,  2,  4,  8], ( 0, [  0,  1,  4,  7]) )
    , ( 13, [  1,  2,  4,  8], ( 0, [  0,  2,  4,  7]) )
    , ( 14, [  1,  2,  4,  8], ( 0, [  0,  2,  4,  8]) )
    , ( 15, [  1,  2,  4,  8], ( 0, [  1,  2,  4,  8]) )
    ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Natural where
    arbitrary = fromIntegral . abs <$> arbitrarySizedIntegral @Int
