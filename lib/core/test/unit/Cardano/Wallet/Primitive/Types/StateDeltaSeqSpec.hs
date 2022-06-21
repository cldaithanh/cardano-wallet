{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeqSpec
    ( spec
    ) where

import Prelude hiding
    ( seq )

import Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq )
import Data.Function
    ( (&) )
import GHC.Generics
    ( Generic )
import Safe
    ( tailMay )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary
    , Fun
    , Function
    , Gen
    , Property
    , applyFun
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , cover
    , genericShrink
    , listOf
    , oneof
    , property
    , shrinkMapBy
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Types.StateDeltaSeq as Seq
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do

    describe "applyDeltas" $ do
        it "prop_applyDeltas_headState" $
            prop_applyDeltas_headState
                & property
        it "prop_applyDeltas_length" $
            prop_applyDeltas_length
                & property

    describe "countEmptyTransitionsWhere" $ do
        it "prop_countEmptyTransitionsWhere_coverage" $
            prop_countEmptyTransitionsWhere_coverage
                & property

    describe "dropEmptyTransitions" $ do
        it "prop_dropEmptyTransitions_toStateList" $
            prop_dropEmptyTransitions_toStateList
                & property

    describe "dropEmptyTransitionWhere" $ do
        it "prop_dropEmptyTransitionWhere_countEmptyTransitionsWhere" $
            prop_dropEmptyTransitionWhere_countEmptyTransitionsWhere
                & property
        it "prop_dropEmptyTransitionWhere_isValid" $
            prop_dropEmptyTransitionWhere_isValid
                & property
        it "prop_dropEmptyTransitionWhere_headState" $
            prop_dropEmptyTransitionWhere_headState
                & property
        it "prop_dropEmptyTransitionWhere_lastState" $
            prop_dropEmptyTransitionWhere_lastState
                & property
        it "prop_dropEmptyTransitionWhere_length" $
            prop_dropEmptyTransitionWhere_length
                & property

    describe "dropEmptyTransitionsWhere" $ do
        it "prop_dropEmptyTransitionsWhere_countEmptyTransitionsWhere" $
            prop_dropEmptyTransitionsWhere_countEmptyTransitionsWhere
                & property
        it "prop_dropEmptyTransitionsWhere_isValid" $
            prop_dropEmptyTransitionsWhere_isValid
                & property
        it "prop_dropEmptyTransitionsWhere_headState" $
            prop_dropEmptyTransitionsWhere_headState
                & property
        it "prop_dropEmptyTransitionsWhere_lastState" $
            prop_dropEmptyTransitionsWhere_lastState
                & property
        it "prop_dropEmptyTransitionsWhere_length" $
            prop_dropEmptyTransitionsWhere_length
                & property

    describe "dropHeads" $ do
        it "prop_dropHeads_head" $
            prop_dropHeads_head
                & property
        it "prop_dropHeads_last" $
            prop_dropHeads_last
                & property
        it "prop_dropHeads_length" $
            prop_dropHeads_length
                & property
        it "prop_dropHeads_isSuffixOf" $
            prop_dropHeads_isSuffixOf
                & property
        it "prop_dropHeads_isValid" $
            prop_dropHeads_isValid
                & property

    describe "dropLasts" $ do
        it "prop_dropLasts_head" $
            prop_dropLasts_head
                & property
        it "prop_dropLasts_last" $
            prop_dropLasts_last
                & property
        it "prop_dropLasts_length" $
            prop_dropLasts_length
                & property
        it "prop_dropLasts_isPrefixOf" $
            prop_dropLasts_isPrefixOf
                & property
        it "prop_dropLasts_isValid" $
            prop_dropLasts_isValid
                & property

    describe "fromState" $ do
        it "prop_fromState_headState" $
            prop_fromState_headState
                & property
        it "prop_fromState_lastState" $
            prop_fromState_lastState
                & property
        it "prop_fromState_length" $
            prop_fromState_length
                & property

    describe "transitions" $ do
        it "prop_transitions_consecutivePairs" $
            prop_transitions_consecutivePairs
                & property
        it "prop_transitions_length" $
            prop_transitions_length
                & property
        it "prop_transitions_nextState" $
            prop_transitions_nextState
                & property
        it "prop_transitions_toDeltaList" $
            prop_transitions_toDeltaList
                & property
        it "prop_transitions_toStateList_initials" $
            prop_transitions_toStateList_initials
                & property
        it "prop_transitions_toStateList_finals" $
            prop_transitions_toStateList_finals
                & property

--------------------------------------------------------------------------------
-- applyDeltas
--------------------------------------------------------------------------------

prop_applyDeltas_headState
    :: TestStateDeltaSeq -> [TestDelta] -> Property
prop_applyDeltas_headState (TestStateDeltaSeq seq) deltas =
    Seq.headState (Seq.applyDeltas applyTestDelta deltas seq)
        === Seq.headState seq

prop_applyDeltas_length
    :: TestStateDeltaSeq -> [TestDelta] -> Property
prop_applyDeltas_length (TestStateDeltaSeq seq) deltas =
    length (Seq.applyDeltas applyTestDelta deltas seq)
        === length seq + length deltas

--------------------------------------------------------------------------------
-- countEmptyTransitionsWhere
--------------------------------------------------------------------------------

prop_countEmptyTransitionsWhere_coverage
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_countEmptyTransitionsWhere_coverage
    (TestStateDeltaSeq seq) (applyFun -> f) =
        checkCoverage $
        cover 10
            (strictlyIncreasing [0, matchCount, emptyCount, length seq])
            "strictlyIncreasing [0, matchCount, emptyCount, length seq]" $
        property True
  where
    emptyCount = Seq.countEmptyTransitions seq
    matchCount = Seq.countEmptyTransitionsWhere f seq

--------------------------------------------------------------------------------
-- dropEmptyTransitions
--------------------------------------------------------------------------------

prop_dropEmptyTransitions_toStateList
    :: TestStateDeltaSeq -> Property
prop_dropEmptyTransitions_toStateList (TestStateDeltaSeq seq) =
        NE.toList (Seq.toStateList $ Seq.dropEmptyTransitions seq)
        === removeConsecutiveDuplicates (NE.toList $ Seq.toStateList seq)

--------------------------------------------------------------------------------
-- dropEmptyTransitionWhere
--------------------------------------------------------------------------------

prop_dropEmptyTransitionWhere_countEmptyTransitionsWhere
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionWhere_countEmptyTransitionsWhere
    (TestStateDeltaSeq seq) (applyFun -> f) =
        all ((== pred emptyTransitionCount) . Seq.countEmptyTransitionsWhere f)
            (Seq.dropEmptyTransitionWhere f seq)
        === True
  where
    emptyTransitionCount = Seq.countEmptyTransitionsWhere f seq

prop_dropEmptyTransitionWhere_isValid
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionWhere_isValid
    (TestStateDeltaSeq seq) (applyFun -> f) =
        all (Seq.isValid applyTestDelta) (Seq.dropEmptyTransitionWhere f seq)
        === True

prop_dropEmptyTransitionWhere_headState
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionWhere_headState
    (TestStateDeltaSeq seq) (applyFun -> f) =
        all ((== Seq.headState seq) . Seq.headState)
            (Seq.dropEmptyTransitionWhere f seq)
        === True

prop_dropEmptyTransitionWhere_lastState
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionWhere_lastState
    (TestStateDeltaSeq seq) (applyFun -> f) =
        all ((== Seq.lastState seq) . Seq.lastState)
            (Seq.dropEmptyTransitionWhere f seq)
        === True

prop_dropEmptyTransitionWhere_length
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionWhere_length
    (TestStateDeltaSeq seq) (applyFun -> f) =
        length (Seq.dropEmptyTransitionWhere f seq)
        === Seq.countEmptyTransitionsWhere f seq

--------------------------------------------------------------------------------
-- dropEmptyTransitionsWhere
--------------------------------------------------------------------------------

prop_dropEmptyTransitionsWhere_countEmptyTransitionsWhere
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionsWhere_countEmptyTransitionsWhere
    (TestStateDeltaSeq seq) (applyFun -> f) =
        Seq.countEmptyTransitionsWhere f (Seq.dropEmptyTransitionsWhere f seq)
        === 0

prop_dropEmptyTransitionsWhere_isValid
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionsWhere_isValid
    (TestStateDeltaSeq seq) (applyFun -> f) =
        Seq.isValid applyTestDelta (Seq.dropEmptyTransitionsWhere f seq)
        === True

prop_dropEmptyTransitionsWhere_headState
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionsWhere_headState
    (TestStateDeltaSeq seq) (applyFun -> f) =
        Seq.headState (Seq.dropEmptyTransitionsWhere f seq)
        === Seq.headState seq

prop_dropEmptyTransitionsWhere_lastState
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionsWhere_lastState
    (TestStateDeltaSeq seq) (applyFun -> f) =
        Seq.lastState (Seq.dropEmptyTransitionsWhere f seq)
        === Seq.lastState seq

prop_dropEmptyTransitionsWhere_length
    :: TestStateDeltaSeq -> Fun TestDelta Bool -> Property
prop_dropEmptyTransitionsWhere_length
    (TestStateDeltaSeq seq) (applyFun -> f) =
        length (Seq.dropEmptyTransitionsWhere f seq)
            + Seq.countEmptyTransitionsWhere f seq
        === length seq

--------------------------------------------------------------------------------
-- dropHeads
--------------------------------------------------------------------------------

prop_dropHeads_head
    :: TestStateDeltaSeq -> Property
prop_dropHeads_head (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.dropHeads seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            NE.head ss === Seq.fromState (Seq.lastState seq)

prop_dropHeads_last
    :: TestStateDeltaSeq -> Property
prop_dropHeads_last (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.dropHeads seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            Just (NE.last ss) === Seq.dropHead seq

prop_dropHeads_length
    :: TestStateDeltaSeq -> Property
prop_dropHeads_length (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.dropHeads seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            NE.length ss === length seq

prop_dropHeads_isSuffixOf
    :: TestStateDeltaSeq -> Property
prop_dropHeads_isSuffixOf (TestStateDeltaSeq seq) =
    all (uncurry Seq.isSuffixOf) (consecutivePairs (Seq.dropHeads seq))
        === True

prop_dropHeads_isValid
    :: TestStateDeltaSeq -> Property
prop_dropHeads_isValid (TestStateDeltaSeq seq) =
    all (Seq.isValid applyTestDelta) (Seq.dropHeads seq)
        === True

--------------------------------------------------------------------------------
-- dropLasts
--------------------------------------------------------------------------------

prop_dropLasts_head
    :: TestStateDeltaSeq -> Property
prop_dropLasts_head (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.dropLasts seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            NE.head ss === Seq.fromState (Seq.headState seq)

prop_dropLasts_last
    :: TestStateDeltaSeq -> Property
prop_dropLasts_last (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.dropLasts seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            Just (NE.last ss) === Seq.dropLast seq

prop_dropLasts_length
    :: TestStateDeltaSeq -> Property
prop_dropLasts_length (TestStateDeltaSeq seq) =
    case NE.nonEmpty (Seq.dropLasts seq) of
        Nothing ->
            length seq === 0
        Just ss ->
            NE.length ss === length seq

prop_dropLasts_isPrefixOf
    :: TestStateDeltaSeq -> Property
prop_dropLasts_isPrefixOf (TestStateDeltaSeq seq) =
    all (uncurry Seq.isPrefixOf) (consecutivePairs (Seq.dropLasts seq))
        === True

prop_dropLasts_isValid
    :: TestStateDeltaSeq -> Property
prop_dropLasts_isValid (TestStateDeltaSeq seq) =
    all (Seq.isValid applyTestDelta) (Seq.dropLasts seq)
        === True

--------------------------------------------------------------------------------
-- fromState
--------------------------------------------------------------------------------

prop_fromState_headState
    :: TestState -> Property
prop_fromState_headState state =
    Seq.headState (Seq.fromState state) === state

prop_fromState_lastState
    :: TestState -> Property
prop_fromState_lastState state =
    Seq.lastState (Seq.fromState state) === state

prop_fromState_length
    :: TestState -> Property
prop_fromState_length state =
    length (Seq.fromState state) === 0

--------------------------------------------------------------------------------
-- transitions
--------------------------------------------------------------------------------

prop_transitions_consecutivePairs
    :: TestStateDeltaSeq -> Property
prop_transitions_consecutivePairs (TestStateDeltaSeq seq) =
    all (\((_, _, sf), (si, _, _)) -> sf == si)
        (consecutivePairs (Seq.transitions seq))
        === True

prop_transitions_length
    :: TestStateDeltaSeq -> Property
prop_transitions_length (TestStateDeltaSeq seq) =
    length (Seq.transitions seq)
        === length seq

prop_transitions_nextState
    :: TestStateDeltaSeq -> Property
prop_transitions_nextState (TestStateDeltaSeq seq) =
    all (\(si, d, sf) -> applyTestDelta si d == sf) (Seq.transitions seq)
        === True

prop_transitions_toDeltaList
    :: TestStateDeltaSeq -> Property
prop_transitions_toDeltaList (TestStateDeltaSeq seq) =
    fmap (\(_, d, _) -> d) (Seq.transitions seq)
        === Seq.toDeltaList seq

prop_transitions_toStateList_initials
    :: TestStateDeltaSeq -> Property
prop_transitions_toStateList_initials (TestStateDeltaSeq seq) =
    fmap (\(si, _, _) -> si) (Seq.transitions seq)
        === NE.take (length seq) (Seq.toStateList seq)

prop_transitions_toStateList_finals
    :: TestStateDeltaSeq -> Property
prop_transitions_toStateList_finals (TestStateDeltaSeq seq) =
    fmap (\(_, _, sf) -> sf) (Seq.transitions seq)
        === NE.drop 1 (Seq.toStateList seq)

--------------------------------------------------------------------------------
-- Test states
--------------------------------------------------------------------------------

newtype TestState = TestState {unTestState :: Int}
    deriving (Eq, Generic, Show)

instance Arbitrary TestState where
    arbitrary = genTestState
    shrink = shrinkTestState

genTestState :: Gen TestState
genTestState = TestState <$> oneof [pure 0, choose (1, 4)]

shrinkTestState :: TestState -> [TestState]
shrinkTestState = shrinkMapBy TestState unTestState shrink

--------------------------------------------------------------------------------
-- Test delta functions
--------------------------------------------------------------------------------

data TestDeltaFn
    = Add
    | Sub
    | Mul
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Arbitrary TestDeltaFn where
    arbitrary = genTestDeltaFn
    shrink = shrinkTestDeltaFn

deriving anyclass instance CoArbitrary TestDeltaFn
deriving anyclass instance Function TestDeltaFn

genTestDeltaFn :: Gen TestDeltaFn
genTestDeltaFn = arbitraryBoundedEnum

shrinkTestDeltaFn :: TestDeltaFn -> [TestDeltaFn]
shrinkTestDeltaFn = genericShrink

applyTestDeltaFn :: TestDeltaFn -> (Int -> Int -> Int)
applyTestDeltaFn = \case
    Add -> (+)
    Sub -> (-)
    Mul -> (*)

--------------------------------------------------------------------------------
-- Test deltas
--------------------------------------------------------------------------------

data TestDelta = TestDelta TestDeltaFn Int
    deriving (Eq, Generic, Show)

instance Arbitrary TestDelta where
    arbitrary = genTestDelta
    shrink = shrinkTestDelta

deriving anyclass instance CoArbitrary TestDelta
deriving anyclass instance Function TestDelta

genTestDelta :: Gen TestDelta
genTestDelta = TestDelta
    <$> genTestDeltaFn
    <*> oneof [pure 0, choose (1, 4)]

shrinkTestDelta :: TestDelta -> [TestDelta]
shrinkTestDelta = genericShrink

applyTestDelta :: TestState -> TestDelta -> TestState
applyTestDelta (TestState i) (TestDelta fn j) =
    TestState $ applyTestDeltaFn fn i j

--------------------------------------------------------------------------------
-- Test state delta sequences
--------------------------------------------------------------------------------

newtype TestStateDeltaSeq = TestStateDeltaSeq
    {unTestStateDeltaSeq :: StateDeltaSeq TestState TestDelta}
    deriving (Eq, Show)

instance Arbitrary TestStateDeltaSeq where
    arbitrary = genTestStateDeltaSeq

genTestStateDeltaSeq :: Gen TestStateDeltaSeq
genTestStateDeltaSeq =
    fmap TestStateDeltaSeq . Seq.fromStateDeltas applyTestDelta
        <$> genTestState
        <*> listOf genTestDelta

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

consecutivePairs :: Foldable f => f a -> [(a, a)]
consecutivePairs (F.toList -> xs) = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys

removeConsecutiveDuplicates :: (Foldable f, Eq a) => f a -> [a]
removeConsecutiveDuplicates = loop . F.toList
  where
    loop = \case
        [ ] -> [ ]
        [a] -> [a]
        (a1 : a2 : as)
            | a1 == a2 -> loop (a2 : as)
            | otherwise -> a1 : loop (a2 : as)

strictlyIncreasing :: (Foldable f, Ord a) => f a -> Bool
strictlyIncreasing as = all (uncurry (<)) (consecutivePairs as)
