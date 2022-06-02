{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeqSpec
    ( spec
    ) where

import Prelude

import Data.Function
    ( (&) )
import Data.Monoid
    ( Sum )
import Safe
    ( tailMay )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Fun, Property, applyFun2, property, (===) )

import qualified Cardano.Wallet.Primitive.Types.StateDeltaSeq as Seq
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do

    describe "fromState" $ do
        it "prop_fromState_headState" $
            prop_fromState_headState
                @(Sum Int) & property
        it "prop_fromState_lastState" $
            prop_fromState_lastState
                @(Sum Int) & property

    describe "appendMany" $ do
        it "prop_fromState_appendMany_headState" $
            prop_fromState_appendMany_headState
                @(Sum Int) @Int & property
        it "prop_fromState_appendMany_size" $
            prop_fromState_appendMany_size
                @(Sum Int) @Int & property

    describe "prefixes" $ do
        it "prop_fromState_appendMany_prefixes_head" $
            prop_fromState_appendMany_prefixes_head
                @(Sum Int) @Int & property
        it "prop_fromState_appendMany_prefixes_last" $
            prop_fromState_appendMany_prefixes_last
                @(Sum Int) @Int & property
        it "prop_fromState_appendMany_prefixes_length" $
            prop_fromState_appendMany_prefixes_length
                @(Sum Int) @Int & property
        it "prop_fromState_appendMany_prefixes_isPrefixOf" $
            prop_fromState_appendMany_prefixes_isPrefixOf
                @(Sum Int) @Int & property

    describe "suffixes" $ do
        it "prop_fromState_appendMany_suffixes_head" $
            prop_fromState_appendMany_suffixes_head
                @(Sum Int) @Int & property
        it "prop_fromState_appendMany_suffixes_last" $
            prop_fromState_appendMany_suffixes_last
                @(Sum Int) @Int & property
        it "prop_fromState_appendMany_suffixes_length" $
            prop_fromState_appendMany_suffixes_length
                @(Sum Int) @Int & property
        it "prop_fromState_appendMany_suffixes_isSuffixOf" $
            prop_fromState_appendMany_suffixes_isSuffixOf
                @(Sum Int) @Int & property

--------------------------------------------------------------------------------
-- fromState
--------------------------------------------------------------------------------

prop_fromState_headState
    :: (Eq state, Show state) => state -> Property
prop_fromState_headState state =
    Seq.headState (Seq.fromState state) === state

prop_fromState_lastState
    :: (Eq state, Show state) => state -> Property
prop_fromState_lastState state =
    Seq.lastState (Seq.fromState state) === state

--------------------------------------------------------------------------------
-- appendMany
--------------------------------------------------------------------------------

prop_fromState_appendMany_headState
    :: (Eq state, Show state)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_headState state nextStateFn deltas =
    Seq.headState result === state
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_fromState_appendMany_size
    :: (Eq state, Show state)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_size state nextStateFn deltas =
    Seq.size result === length deltas
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

--------------------------------------------------------------------------------
-- prefixes
--------------------------------------------------------------------------------

prop_fromState_appendMany_prefixes_head
    :: (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_prefixes_head state nextStateFn deltas =
    NE.head (Seq.prefixes result) === Seq.fromState state
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_fromState_appendMany_prefixes_last
    :: (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_prefixes_last state nextStateFn deltas =
    NE.last (Seq.prefixes result) === result
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_fromState_appendMany_prefixes_length
    :: (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_prefixes_length state nextStateFn deltas =
    NE.length (Seq.prefixes result) === length deltas + 1
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_fromState_appendMany_prefixes_isPrefixOf
    :: (Eq state, Show state, Eq delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_prefixes_isPrefixOf state nextStateFn deltas =
    all (uncurry Seq.isPrefixOf) (consecutivePairs (Seq.prefixes result))
        === True
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

--------------------------------------------------------------------------------
-- suffixes
--------------------------------------------------------------------------------

prop_fromState_appendMany_suffixes_head
    :: (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_suffixes_head state nextStateFn deltas =
    NE.head (Seq.suffixes result) === Seq.fromState (Seq.lastState result)
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_fromState_appendMany_suffixes_last
    :: (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_suffixes_last state nextStateFn deltas =
    NE.last (Seq.suffixes result) === result
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_fromState_appendMany_suffixes_length
    :: (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_suffixes_length state nextStateFn deltas =
    NE.length (Seq.suffixes result) === length deltas + 1
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_fromState_appendMany_suffixes_isSuffixOf
    :: (Eq state, Show state, Eq delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_suffixes_isSuffixOf state nextStateFn deltas =
    all (uncurry Seq.isSuffixOf) (consecutivePairs (Seq.suffixes result))
        === True
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

consecutivePairs :: Foldable f => f a -> [(a, a)]
consecutivePairs (F.toList -> xs) = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys
