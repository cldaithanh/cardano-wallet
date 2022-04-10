{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.MonoidMapSpec
    where

import Prelude

import Algebra.Difference
    ( laws_Difference_Eq_Monoid
    , laws_Difference_PartialOrd
    , laws_Difference_PartialOrd_Monoid
    , laws_Difference_PartialOrd_Semigroup
    )
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
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), arbitrarySizedIntegral, listOf )
import Test.QuickCheck.Classes
    ( eqLaws
    , isListLaws
    , monoidLaws
    , semigroupLaws
    , semigroupMonoidLaws
    , showReadLaws
    )
import Test.Utils.Laws
    ( testLawsMany )
import Test.Utils.Laws.PartialOrd
    ( partialOrdLaws )

spec :: Spec
spec =
    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(MonoidMap Int (Sum Natural))
            [ eqLaws
            , isListLaws
            , monoidLaws
            , partialOrdLaws
            , partitionLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            , subtractLaws
            , subtractPartialOrdLaws
            , laws_Difference_Eq_Monoid
            , laws_Difference_PartialOrd
            , laws_Difference_PartialOrd_Semigroup
            , laws_Difference_PartialOrd_Monoid
            ]
        testLawsMany @(Keys (MonoidMap Int (Sum Natural)))
            [ equipartitionLaws
            ]
        testLawsMany @(Values (MonoidMap Int (Sum Natural)))
            [ equipartitionLaws
            ]

instance Arbitrary Natural where
    arbitrary = fromIntegral . abs <$> arbitrarySizedIntegral @Int

deriving instance Arbitrary a => Arbitrary (Keys a)
deriving instance Arbitrary a => Arbitrary (Values a)

instance (Arbitrary k, Ord k, Arbitrary v, Eq v, Monoid v) =>
    Arbitrary (MonoidMap k v)
  where
    arbitrary = fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)
