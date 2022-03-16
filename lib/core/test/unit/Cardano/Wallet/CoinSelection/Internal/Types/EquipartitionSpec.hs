{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.Internal.Types.EquipartitionSpec
    where

import Prelude

import Cardano.Wallet.CoinSelection.Internal.Types.Equipartition
    ( Equipartition (..)
    , bipartition
    , bipartitionUntil
    , bipartitionWhile
    , equipartitionLaws
    )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Fun
    , Property
    , Testable
    , applyFun
    , arbitrarySizedIntegral
    , checkCoverage
    , cover
    , property
    , (===)
    )
import Test.Utils.Laws
    ( testLawsMany )

import qualified Data.Foldable as F

spec :: Spec
spec = do

    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(Sum Natural)
            [ equipartitionLaws
            ]
        testLawsMany @[Int]
            [ equipartitionLaws
            ]
        testLawsMany @(Map Int Int)
            [ equipartitionLaws
            ]
        testLawsMany @(Set Int)
            [ equipartitionLaws
            ]

    parallel $ describe "Bipartitioning" $ do

        describe "bipartitionUntil_mempty" $ do
            it "bipartitionUntil_mempty @[Int]" $
                property $ prop_bipartitionUntil_mempty @[Int]
            it "bipartitionUntil_mempty @(Map Int Int)" $
                property $ prop_bipartitionUntil_mempty @(Map Int Int)

        describe "bipartitionUntil_idempotent" $ do
            it "bipartitionUntil_idempotent @[Int]" $
                property $ prop_bipartitionUntil_idempotent @[Int]
            it "bipartitionUntil_idempotent @(Map Int Int)" $
                property $ prop_bipartitionUntil_idempotent @(Map Int Int)

        describe "bipartitionUntil_satisfy" $ do
            it "bipartitionUntil_satisfy @[Int]" $
                property $ prop_bipartitionUntil_satisfy @[Int]
            it "bipartitionUntil_satisfy @(Map Int Int)" $
                property $ prop_bipartitionUntil_satisfy @(Map Int Int)

        describe "bipartitionUntil_sum" $ do
            it "bipartitionUntil_sum @[Int]" $
                property $ prop_bipartitionUntil_sum @[Int]
            it "bipartitionUntil_sum @(Map Int Int)" $
                property $ prop_bipartitionUntil_sum @(Map Int Int)

        describe "bipartitionUntil_true" $ do
            it "bipartitionUntil_true @[Int]" $
                property $ prop_bipartitionUntil_true @[Int]
            it "bipartitionUntil_true @(Map Int Int)" $
                property $ prop_bipartitionUntil_true @(Map Int Int)

        describe "bipartitionUntil_bipartitionWhile" $ do
            it "bipartitionUntil_bipartitionWhile @[Int]" $
                property $ prop_bipartitionUntil_bipartitionWhile @[Int]
            it "bipartitionUntil_bipartitionWhile @(Map Int Int)" $
                property $ prop_bipartitionUntil_bipartitionWhile @(Map Int Int)

--------------------------------------------------------------------------------
-- Bipartitioning
--------------------------------------------------------------------------------

prop_bipartitionUntil_coverage
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a, Testable prop)
    => a
    -> Fun a Bool
    -> prop
    -> Property
prop_bipartitionUntil_coverage a f prop
    = checkCoverage
    $ cover 2
        (a == mempty)
        "a == mempty"
    $ cover 20
        (a /= mempty)
        "a /= mempty"
    $ cover 20
        (applyFun f a)
        "applyFun f a"
    $ cover 20
        (applyFun f mempty)
        "applyFun f mempty"
    $ cover 20
        (not (applyFun f mempty))
        "not (applyFun f mempty)"
    $ cover 20
        (not (applyFun f a))
        "not (applyFun f a)"
    $ cover 20
        (F.length result == 1)
        "F.length result == 1"
    $ cover 5
        (F.length result == 2)
        "F.length result == 2"
    $ cover 5
        (F.length result >= 3)
        "F.length result >= 3"
    $ property prop
  where
    result = bipartitionUntil a (applyFun f)

prop_bipartitionUntil_mempty
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Fun a Bool
    -> Property
prop_bipartitionUntil_mempty a f =
    prop_bipartitionUntil_coverage a f $
    if a == mempty
    then result === pure mempty
    else property $ mempty `notElem` result
  where
    result = bipartitionUntil a (applyFun f)

prop_bipartitionUntil_idempotent
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Fun a Bool
    -> Property
prop_bipartitionUntil_idempotent a f =
    prop_bipartitionUntil_coverage a f $
    (flip bipartitionUntil (applyFun f) =<< result) === result
  where
    result = bipartitionUntil a (applyFun f)

prop_bipartitionUntil_satisfy
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Fun a Bool
    -> Property
prop_bipartitionUntil_satisfy a f =
    prop_bipartitionUntil_coverage a f $
    all satisfiesCondition (bipartitionUntil a (applyFun f))
  where
    satisfiesCondition x = applyFun f x || bipartition x == (mempty, x)

prop_bipartitionUntil_sum
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Fun a Bool
    -> Property
prop_bipartitionUntil_sum a f =
    prop_bipartitionUntil_coverage a f $
    F.fold (bipartitionUntil a (applyFun f)) === a

prop_bipartitionUntil_true
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Property
prop_bipartitionUntil_true a =
    bipartitionUntil a (const True) === pure a

prop_bipartitionUntil_bipartitionWhile
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Fun a Bool
    -> Property
prop_bipartitionUntil_bipartitionWhile a f =
    prop_bipartitionUntil_coverage a f $
    bipartitionUntil a (applyFun f) === bipartitionWhile a (not . applyFun f)

--------------------------------------------------------------------------------
-- Arbitraries
--------------------------------------------------------------------------------

newtype Sum a = Sum a
    deriving (Arbitrary, Eq, Equipartition, Show)

instance Monoid (Sum Natural) where
    mempty = Sum 0

instance Semigroup (Sum Natural) where
    Sum n1 <> Sum n2 = Sum (n1 + n2)

instance Arbitrary Natural where
    arbitrary = fromIntegral . abs <$> arbitrarySizedIntegral @Int
