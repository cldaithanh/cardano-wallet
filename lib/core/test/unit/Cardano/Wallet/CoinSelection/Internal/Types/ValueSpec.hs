{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.Internal.Types.ValueSpec
    where

import Prelude

import Cardano.Wallet.CoinSelection.Internal.Types.Difference
    ( differenceLaws, differenceOrdLaws )
import Cardano.Wallet.CoinSelection.Internal.Types.Equipartition
    ( equipartitionLaws )
import Cardano.Wallet.CoinSelection.Internal.Types.Partition
    ( partitionLaws )
import Cardano.Wallet.CoinSelection.Internal.Types.Subtract
    ( subtractLaws, subtractOrdLaws )
import Cardano.Wallet.CoinSelection.Internal.Types.Value
    ( Value (..) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
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
            [ differenceLaws
            , differenceOrdLaws
            , equipartitionLaws
            , eqLaws
            , monoidLaws
            , ordLaws
            , partitionLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showReadLaws
            , subtractLaws
            , subtractOrdLaws
            ]

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary

instance Arbitrary Value where
    arbitrary = Value . fromIntegral . abs <$> arbitrarySizedIntegral @Int
