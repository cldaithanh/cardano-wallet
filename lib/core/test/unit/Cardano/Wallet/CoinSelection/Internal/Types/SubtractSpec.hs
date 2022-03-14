{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.Internal.Types.SubtractSpec
    where

import Prelude

import Cardano.Wallet.CoinSelection.Internal.Types.Subtract
    ( Subtract (..), subtractLaws, subtractOrdLaws )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), arbitrarySizedIntegral )
import Test.Utils.Laws
    ( testLawsMany )

spec :: Spec
spec =
    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(Sum Natural)
            [ subtractLaws
            , subtractOrdLaws
            ]
        testLawsMany @(Set Int)
            [ subtractLaws
            ]

newtype Sum a = Sum a
    deriving (Arbitrary, Eq, Ord, Show, Subtract)

instance Monoid (Sum Natural) where
    mempty = Sum 0

instance Semigroup (Sum Natural) where
    Sum n1 <> Sum n2 = Sum (n1 + n2)

instance Arbitrary Natural where
    arbitrary = fromIntegral . abs <$> arbitrarySizedIntegral @Int
