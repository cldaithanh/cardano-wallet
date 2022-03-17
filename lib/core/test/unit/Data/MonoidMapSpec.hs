{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.MonoidMapSpec
    where

import Prelude

import Algebra.Difference
    ( differenceLaws, differencePartialOrdLaws )
import Algebra.Equipartition
    ( equipartitionLaws )
import Algebra.Partition
    ( partitionLaws )
import Algebra.Subtract
    ( subtractLaws, subtractPartialOrdLaws )
import Data.Monoid
    ( Sum (..) )
import Data.MonoidMap
    ( Keys (..), MonoidMap, Values (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), arbitrarySizedIntegral, listOf )
import Test.QuickCheck.Classes
    ( eqLaws, monoidLaws, semigroupLaws, semigroupMonoidLaws, showReadLaws )
import Test.Utils.Laws
    ( testLawsMany )
import Test.Utils.Laws.PartialOrd
    ( partialOrdLaws )

import qualified Data.MonoidMap as MonoidMap

spec :: Spec
spec =
    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(MonoidMap Int (Sum Natural))
            [ differenceLaws
            , differencePartialOrdLaws
            , eqLaws
            , monoidLaws
            , partialOrdLaws
            , partitionLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            , subtractLaws
            , subtractPartialOrdLaws
            ]
        testLawsMany @(Keys (MonoidMap Int (Sum Natural)))
            [ equipartitionLaws
            ]
        testLawsMany @(Values (MonoidMap Int (Sum Natural)))
            [ equipartitionLaws
            ]

instance Arbitrary Natural where
    arbitrary = fromIntegral . abs <$> arbitrarySizedIntegral @Int

instance Arbitrary a => Arbitrary (Keys a) where
    arbitrary = Keys <$> arbitrary

instance Arbitrary a => Arbitrary (Values a) where
    arbitrary = Values <$> arbitrary

instance (Arbitrary k, Ord k, Arbitrary v, Eq v, Monoid v) =>
    Arbitrary (MonoidMap k v)
  where
    arbitrary = MonoidMap.fromSequence
        <$> listOf ((,) <$> arbitrary <*> arbitrary)
