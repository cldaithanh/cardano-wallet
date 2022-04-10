{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.Internal.Types.ValueSpec
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
    ( subtractLaws, subtractOrdLaws )
import Cardano.Wallet.CoinSelection.Internal.Types.Value
    ( Value (..) )
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), arbitrarySizedIntegral )
import Test.QuickCheck.Classes
    ( eqLaws
    , monoidLaws
    , ordLaws
    , semigroupLaws
    , semigroupMonoidLaws
    , showReadLaws
    )
import Test.Utils.Laws
    ( testLawsMany )

spec :: Spec
spec =
    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @Value
            [ equipartitionLaws
            , eqLaws
            , monoidLaws
            , ordLaws
            , partitionLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            , subtractLaws
            , subtractOrdLaws
            , laws_Difference_Eq_Monoid
            , laws_Difference_PartialOrd
            , laws_Difference_PartialOrd_Semigroup
            , laws_Difference_PartialOrd_Monoid
            ]

instance Arbitrary Value where
    arbitrary = Value . fromIntegral . abs <$> arbitrarySizedIntegral @Int
