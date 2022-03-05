{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.Internal.Types.ValueMapSpec
    where

import Prelude

import Cardano.Wallet.CoinSelection.Internal.Types.Difference
    ( differenceLaws, differencePartialOrdLaws )
import Cardano.Wallet.CoinSelection.Internal.Types.Equipartition
    ( equipartitionLaws )
import Cardano.Wallet.CoinSelection.Internal.Types.Partition
    ( partitionLaws )
import Cardano.Wallet.CoinSelection.Internal.Types.Subtract
    ( subtractLaws, subtractPartialOrdLaws )
import Cardano.Wallet.CoinSelection.Internal.Types.Value
    ( Value (..) )
import Cardano.Wallet.CoinSelection.Internal.Types.ValueMap
    ( Keys (..), ValueMap (..), Values (..) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
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

import qualified Cardano.Wallet.CoinSelection.Internal.Types.ValueMap as ValueMap

spec :: Spec
spec =
    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(ValueMap Int Value)
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
        testLawsMany @(Keys (ValueMap Int Value))
            [ equipartitionLaws
            ]
        testLawsMany @(Values (ValueMap Int Value))
            [ equipartitionLaws
            ]

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary

instance Arbitrary Value where
    arbitrary = Value . fromIntegral . abs <$> arbitrarySizedIntegral @Int

instance Arbitrary a => Arbitrary (Keys a) where
    arbitrary = Keys <$> arbitrary

instance Arbitrary a => Arbitrary (Values a) where
    arbitrary = Values <$> arbitrary

instance (Arbitrary k, Ord k, Arbitrary v, Eq v, Monoid v) =>
    Arbitrary (ValueMap k v)
  where
    arbitrary = ValueMap.fromSequence
        <$> listOf ((,) <$> arbitrary <*> arbitrary)
