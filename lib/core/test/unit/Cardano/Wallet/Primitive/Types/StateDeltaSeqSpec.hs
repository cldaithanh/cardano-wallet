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
    ( Fun, Property, (===), applyFun2, property )

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
        it "prop_fromState_appendMany_headState" $
            prop_fromState_appendMany_headState
                @(Sum Int) @Int & property
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
        it "prop_fromState_appendMany_size" $
            prop_fromState_appendMany_size
                @(Sum Int) @Int & property

prop_fromState_headState
    :: (Eq state, Show state) => state -> Property
prop_fromState_headState state =
    Seq.headState (Seq.fromState state) === state

prop_fromState_lastState
    :: (Eq state, Show state) => state -> Property
prop_fromState_lastState state =
    Seq.lastState (Seq.fromState state) === state

prop_fromState_appendMany_headState
    :: forall state delta. (Eq state, Show state)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_headState headState nextStateFn deltas =
    Seq.headState result === headState
  where
    initialSeq = Seq.fromState headState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

--------------------------------------------------------------------------------
-- Longest proper suffix
--------------------------------------------------------------------------------

-- prop_fromState_appendMany_longestProperSuffix_isSuffix

--------------------------------------------------------------------------------
-- Prefixes
--------------------------------------------------------------------------------

prop_fromState_appendMany_prefixes_head
    :: forall state delta. (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_prefixes_head
    headState nextStateFn deltas =
        NE.head (Seq.prefixes result) === initialSeq
  where
    initialSeq = Seq.fromState headState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

prop_fromState_appendMany_prefixes_last
    :: forall state delta. (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_prefixes_last
    headState nextStateFn deltas =
        NE.last (Seq.prefixes result) === result
  where
    initialSeq = Seq.fromState headState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

prop_fromState_appendMany_prefixes_length
    :: forall state delta. (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_prefixes_length
    headState nextStateFn deltas =
        NE.length (Seq.prefixes result) === length deltas + 1
  where
    initialSeq = Seq.fromState headState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

prop_fromState_appendMany_prefixes_isPrefixOf
    :: forall state delta. (Eq state, Show state, Eq delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_prefixes_isPrefixOf
    headState nextStateFn deltas =
        all (uncurry Seq.isPrefixOf) (consecutivePairs (Seq.prefixes result))
            === True
  where
    initialSeq = Seq.fromState headState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

--------------------------------------------------------------------------------
-- Suffixes
--------------------------------------------------------------------------------

prop_fromState_appendMany_suffixes_head
    :: forall state delta. (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_suffixes_head headState nextStateFn deltas =
    NE.head (Seq.suffixes result)
        === Seq.fromState (Seq.lastState result)
  where
    initialSeq = Seq.fromState headState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

prop_fromState_appendMany_suffixes_last
    :: forall state delta. (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_suffixes_last headState nextStateFn deltas =
    NE.last (Seq.suffixes result) === result
  where
    initialSeq = Seq.fromState headState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

prop_fromState_appendMany_suffixes_length
    :: forall state delta. (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_suffixes_length
    headState nextStateFn deltas =
        NE.length (Seq.suffixes result) === length deltas + 1
  where
    initialSeq = Seq.fromState headState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

prop_fromState_appendMany_suffixes_isSuffixOf
    :: forall state delta. (Eq state, Show state, Eq delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_suffixes_isSuffixOf headState nextStateFn deltas =
    all (uncurry Seq.isSuffixOf) (consecutivePairs (Seq.suffixes result))
        === True
  where
    initialSeq = Seq.fromState headState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

--------------------------------------------------------------------------------

prop_fromState_appendMany_size
    :: forall state delta. (Eq state, Show state)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromState_appendMany_size headState nextStateFn deltas =
    Seq.size result === length deltas
  where
    initialSeq = Seq.fromState headState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

consecutivePairs :: Foldable f => f a -> [(a, a)]
consecutivePairs (F.toList -> xs) = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys
