{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.EquipartitionSpec
    where

import Prelude

import Algebra.Equipartition
    ( Equipartition (..)
    , bipartition
    , bipartitionUntil
    , bipartitionWhile
    , equipartitionLaws
    , equipartitionN
    )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.Hspec.Unit
    ( UnitTestData2
    , unitTestData2
    , unitTestSpec
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , pattern Fn
    , Fun
    , Property
    , Testable
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

    parallel $ describe "bipartition" $ do

        describe "bipartitionUntil_const" $ do
            it "bipartitionUntil_const @[Int]" $
                property $ prop_bipartitionUntil_const @[Int]
            it "bipartitionUntil_const @(Map Int Int)" $
                property $ prop_bipartitionUntil_const @(Map Int Int)

        describe "bipartitionUntil_idempotent" $ do
            it "bipartitionUntil_idempotent @[Int]" $
                property $ prop_bipartitionUntil_idempotent @[Int]
            it "bipartitionUntil_idempotent @(Map Int Int)" $
                property $ prop_bipartitionUntil_idempotent @(Map Int Int)

        describe "bipartitionUntil_mempty" $ do
            it "bipartitionUntil_mempty @[Int]" $
                property $ prop_bipartitionUntil_mempty @[Int]
            it "bipartitionUntil_mempty @(Map Int Int)" $
                property $ prop_bipartitionUntil_mempty @(Map Int Int)

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

        describe "bipartitionUntil_bipartitionWhile" $ do
            it "bipartitionUntil_bipartitionWhile @[Int]" $
                property $ prop_bipartitionUntil_bipartitionWhile @[Int]
            it "bipartitionUntil_bipartitionWhile @(Map Int Int)" $
                property $ prop_bipartitionUntil_bipartitionWhile @(Map Int Int)

    parallel $ describe "equipartitionN" $ do

        describe "unit tests" $ do
            unitTestSpec_equipartitionN_Natural
            unitTestSpec_equipartitionN_Set
            unitTestSpec_equipartitionN_Map

--------------------------------------------------------------------------------
-- Bipartitioning
--------------------------------------------------------------------------------

prop_bipartitionUntil_coverage
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a, Testable prop)
    => a
    -> (a -> Bool)
    -> prop
    -> Property
prop_bipartitionUntil_coverage value condition prop
    = checkCoverage
    $ cover 2
        (value == mempty)
        "value == mempty"
    $ cover 20
        (value /= mempty)
        "value /= mempty"
    $ cover 20
        (condition value)
        "condition value"
    $ cover 20
        (condition mempty)
        "condition mempty"
    $ cover 20
        (not (condition mempty))
        "not (condition mempty)"
    $ cover 20
        (not (condition value))
        "not (condition value)"
    $ cover 20
        (F.length result == 1)
        "F.length result == 1"
    $ cover 1
        (F.length result == 2)
        "F.length result == 2"
    $ cover 5
        (F.length result >= 3)
        "F.length result >= 3"
    $ property prop
  where
    result = value `bipartitionUntil` condition

prop_bipartitionUntil_const
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Bool
    -> Property
prop_bipartitionUntil_const a condition =
    prop_bipartitionUntil_coverage a (const condition) $
    result ===
        if condition
        then pure a
        else equipartition a result
  where
    result = a `bipartitionUntil` (const condition)

prop_bipartitionUntil_idempotent
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Fun a Bool
    -> Property
prop_bipartitionUntil_idempotent a (Fn f) =
    prop_bipartitionUntil_coverage a f $
    ((`bipartitionUntil` f) =<< result) === result
  where
    result = a `bipartitionUntil` f

prop_bipartitionUntil_mempty
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Fun a Bool
    -> Property
prop_bipartitionUntil_mempty a (Fn f) =
    prop_bipartitionUntil_coverage a f $
    if a == mempty
    then result === pure mempty
    else property $ mempty `notElem` result
  where
    result = a `bipartitionUntil` f

prop_bipartitionUntil_satisfy
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Fun a Bool
    -> Property
prop_bipartitionUntil_satisfy a (Fn f) =
    prop_bipartitionUntil_coverage a f $
    all satisfiesCondition (a `bipartitionUntil` f)
  where
    satisfiesCondition x = f x || bipartition x == (mempty, x)

prop_bipartitionUntil_sum
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Fun a Bool
    -> Property
prop_bipartitionUntil_sum a (Fn f) =
    prop_bipartitionUntil_coverage a f $
    F.fold (a `bipartitionUntil` f) === a

prop_bipartitionUntil_bipartitionWhile
    :: (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => a
    -> Fun a Bool
    -> Property
prop_bipartitionUntil_bipartitionWhile a (Fn f) =
    prop_bipartitionUntil_coverage a f $
    a `bipartitionUntil` f === a `bipartitionWhile` (not . f)

--------------------------------------------------------------------------------
-- Unit tests: Equipartition Natural
--------------------------------------------------------------------------------

unitTestSpec_equipartitionN_Natural :: Spec
unitTestSpec_equipartitionN_Natural = unitTestSpec
    "equipartitionN Natural"
    "equipartitionN"
    (equipartitionN)
    (unitTestData_equipartitionN_Natural)

unitTestData_equipartitionN_Natural :: UnitTestData2
    (Natural)
    (Int)
    (NonEmpty Natural)
unitTestData_equipartitionN_Natural = unitTestData2
    [ ( 0,  1, [                                     0])
    , ( 0,  2, [                                 0,  0])
    , ( 0,  3, [                             0,  0,  0])
    , ( 0,  4, [                         0,  0,  0,  0])
    , ( 0,  5, [                     0,  0,  0,  0,  0])
    , ( 0,  6, [                 0,  0,  0,  0,  0,  0])
    , ( 0,  7, [             0,  0,  0,  0,  0,  0,  0])
    , ( 0,  8, [         0,  0,  0,  0,  0,  0,  0,  0])
    , ( 0,  9, [     0,  0,  0,  0,  0,  0,  0,  0,  0])
    , ( 0, 10, [ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0])

    , ( 1,  1, [                                     1])
    , ( 1,  2, [                                 0,  1])
    , ( 1,  3, [                             0,  0,  1])
    , ( 1,  4, [                         0,  0,  0,  1])
    , ( 1,  5, [                     0,  0,  0,  0,  1])
    , ( 1,  6, [                 0,  0,  0,  0,  0,  1])
    , ( 1,  7, [             0,  0,  0,  0,  0,  0,  1])
    , ( 1,  8, [         0,  0,  0,  0,  0,  0,  0,  1])
    , ( 1,  9, [     0,  0,  0,  0,  0,  0,  0,  0,  1])
    , ( 1, 10, [ 0,  0,  0,  0,  0,  0,  0,  0,  0,  1])

    , ( 2,  1, [                                     2])
    , ( 2,  2, [                                 1,  1])
    , ( 2,  3, [                             0,  1,  1])
    , ( 2,  4, [                         0,  0,  1,  1])
    , ( 2,  5, [                     0,  0,  0,  1,  1])
    , ( 2,  6, [                 0,  0,  0,  0,  1,  1])
    , ( 2,  7, [             0,  0,  0,  0,  0,  1,  1])
    , ( 2,  8, [         0,  0,  0,  0,  0,  0,  1,  1])
    , ( 2,  9, [     0,  0,  0,  0,  0,  0,  0,  1,  1])
    , ( 2, 10, [ 0,  0,  0,  0,  0,  0,  0,  0,  1,  1])

    , ( 9,  1, [                                     9])
    , ( 9,  2, [                                 4,  5])
    , ( 9,  3, [                             3,  3,  3])
    , ( 9,  4, [                         2,  2,  2,  3])
    , ( 9,  5, [                     1,  2,  2,  2,  2])
    , ( 9,  6, [                 1,  1,  1,  2,  2,  2])
    , ( 9,  7, [             1,  1,  1,  1,  1,  2,  2])
    , ( 9,  8, [         1,  1,  1,  1,  1,  1,  1,  2])
    , ( 9,  9, [     1,  1,  1,  1,  1,  1,  1,  1,  1])
    , ( 9, 10, [ 0,  1,  1,  1,  1,  1,  1,  1,  1,  1])

    , (10,  1, [                                    10])
    , (10,  2, [                                 5,  5])
    , (10,  3, [                             3,  3,  4])
    , (10,  4, [                         2,  2,  3,  3])
    , (10,  5, [                     2,  2,  2,  2,  2])
    , (10,  6, [                 1,  1,  2,  2,  2,  2])
    , (10,  7, [             1,  1,  1,  1,  2,  2,  2])
    , (10,  8, [         1,  1,  1,  1,  1,  1,  2,  2])
    , (10,  9, [     1,  1,  1,  1,  1,  1,  1,  1,  2])
    , (10, 10, [ 1,  1,  1,  1,  1,  1,  1,  1,  1,  1])

    , (99,  1, [                                    99])
    , (99,  2, [                                49, 50])
    , (99,  3, [                            33, 33, 33])
    , (99,  4, [                        24, 25, 25, 25])
    , (99,  5, [                    19, 20, 20, 20, 20])
    , (99,  6, [                16, 16, 16, 17, 17, 17])
    , (99,  7, [            14, 14, 14, 14, 14, 14, 15])
    , (99,  8, [        12, 12, 12, 12, 12, 13, 13, 13])
    , (99,  9, [    11, 11, 11, 11, 11, 11, 11, 11, 11])
    , (99, 10, [ 9, 10, 10, 10, 10, 10, 10, 10, 10, 10])
    ]

--------------------------------------------------------------------------------
-- Unit tests: equipartitionN Set
--------------------------------------------------------------------------------

unitTestSpec_equipartitionN_Set :: Spec
unitTestSpec_equipartitionN_Set = unitTestSpec
    "equipartitionN Set"
    "equipartitionN"
    (equipartitionN)
    (unitTestData_equipartitionN_Set)

unitTestData_equipartitionN_Set :: UnitTestData2
    (Set LatinChar)
    (Int)
    (NonEmpty (Set LatinChar))
unitTestData_equipartitionN_Set = unitTestData2
    [ (s, 1, [ [A ,  B ,  C ,  D ,  E ,  F ,  G ,  H] ])
    , (s, 2, [ [A ,  B ,  C ,  D], [E ,  F ,  G ,  H] ])
    , (s, 3, [ [A ,  B], [C ,  D ,  E], [F ,  G ,  H] ])
    , (s, 4, [ [A ,  B], [C ,  D], [E ,  F], [G ,  H] ])
    , (s, 5, [ [A], [B], [C ,  D], [E ,  F], [G ,  H] ])
    , (s, 6, [ [A], [B], [C], [D], [E ,  F], [G ,  H] ])
    , (s, 7, [ [A], [B], [C], [D], [E], [F], [G ,  H] ])
    , (s, 8, [ [A], [B], [C], [D], [E], [F], [G], [H] ])
    ]
  where
    s = [A .. H]

--------------------------------------------------------------------------------
-- Unit tests: equipartitionN Map
--------------------------------------------------------------------------------

unitTestSpec_equipartitionN_Map :: Spec
unitTestSpec_equipartitionN_Map = unitTestSpec
    "equipartitionN Map"
    "equipartitionN"
    (equipartitionN)
    (unitTestData_equipartitionN_Map)

unitTestData_equipartitionN_Map :: UnitTestData2
    (Map LatinChar Int)
    (Int)
    (NonEmpty (Map LatinChar Int))
unitTestData_equipartitionN_Map = unitTestData2
    [ (m, 1, [ [A➔1 ,  B➔2 ,  C➔3 ,  D➔4 ,  E➔5 ,  F➔6 ,  G➔7 ,  H➔8] ])
    , (m, 2, [ [A➔1 ,  B➔2 ,  C➔3 ,  D➔4], [E➔5 ,  F➔6 ,  G➔7 ,  H➔8] ])
    , (m, 3, [ [A➔1 ,  B➔2], [C➔3 ,  D➔4 ,  E➔5], [F➔6 ,  G➔7 ,  H➔8] ])
    , (m, 4, [ [A➔1 ,  B➔2], [C➔3 ,  D➔4], [E➔5 ,  F➔6], [G➔7 ,  H➔8] ])
    , (m, 5, [ [A➔1], [B➔2], [C➔3 ,  D➔4], [E➔5 ,  F➔6], [G➔7 ,  H➔8] ])
    , (m, 6, [ [A➔1], [B➔2], [C➔3], [D➔4], [E➔5 ,  F➔6], [G➔7 ,  H➔8] ])
    , (m, 7, [ [A➔1], [B➔2], [C➔3], [D➔4], [E➔5], [F➔6], [G➔7 ,  H➔8] ])
    , (m, 8, [ [A➔1], [B➔2], [C➔3], [D➔4], [E➔5], [F➔6], [G➔7], [H➔8] ])
    ]
  where
    m :: Map LatinChar Int
    m = fromList $ zipWith (➔) [A .. H] [1 ..]

    (➔) :: a -> b -> (a, b)
    (➔) = (,)

--------------------------------------------------------------------------------
-- Latin characters
--------------------------------------------------------------------------------

data LatinChar
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

newtype Sum a = Sum a
    deriving (Arbitrary, Eq, Equipartition, Show)

instance Monoid (Sum Natural) where
    mempty = Sum 0

instance Semigroup (Sum Natural) where
    Sum n1 <> Sum n2 = Sum (n1 + n2)

instance Arbitrary Natural where
    arbitrary = fromIntegral . abs <$> arbitrarySizedIntegral @Int
