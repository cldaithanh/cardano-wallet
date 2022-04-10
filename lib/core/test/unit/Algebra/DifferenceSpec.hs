{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Algebra.DifferenceSpec
    where

import Prelude

import Algebra.Difference
    ( Difference (..)
    , laws_Difference_Eq_Monoid
    , laws_Difference_PartialOrd
    , laws_Difference_PartialOrd_Monoid
    , laws_Difference_PartialOrd_Semigroup
    )
import Algebra.Lattice.Ordered
    ( Ordered (..) )
import Algebra.PartialOrd
    ( PartialOrd (..) )
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
            [ laws_Difference_Eq_Monoid
            , laws_Difference_PartialOrd
            , laws_Difference_PartialOrd_Semigroup
            , laws_Difference_PartialOrd_Monoid
            ]
        testLawsMany @(Set Int)
            [ laws_Difference_Eq_Monoid
            , laws_Difference_PartialOrd
            , laws_Difference_PartialOrd_Semigroup
            , laws_Difference_PartialOrd_Monoid
            ]

newtype Sum a = Sum a
    deriving (Arbitrary, Difference, Eq, Ord, Show)
    deriving PartialOrd via (Ordered a)

instance Monoid (Sum Natural) where
    mempty = Sum 0

instance Semigroup (Sum Natural) where
    Sum n1 <> Sum n2 = Sum (n1 + n2)

instance Arbitrary Natural where
    arbitrary = fromIntegral . abs <$> arbitrarySizedIntegral @Int
