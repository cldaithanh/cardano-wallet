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
        it "prop_appendMany_headState" $
            prop_appendMany_headState
                @(Sum Int) @Int & property
        it "prop_appendMany_size" $
            prop_appendMany_size
                @(Sum Int) @Int & property

    describe "dropHeads" $ do
        it "prop_dropHeads_head" $
            prop_dropHeads_head
                @(Sum Int) @Int & property
        it "prop_dropHeads_last" $
            prop_dropHeads_last
                @(Sum Int) @Int & property
        it "prop_dropHeads_length" $
            prop_dropHeads_length
                @(Sum Int) @Int & property
        it "prop_dropHeads_isSuffixOf" $
            prop_dropHeads_isSuffixOf
                @(Sum Int) @Int & property

    describe "dropLasts" $ do
        it "prop_dropLasts_head" $
            prop_dropLasts_head
                @(Sum Int) @Int & property
        it "prop_dropLasts_last" $
            prop_dropLasts_last
                @(Sum Int) @Int & property
        it "prop_dropLasts_length" $
            prop_dropLasts_length
                @(Sum Int) @Int & property
        it "prop_dropLasts_isPrefixOf" $
            prop_dropLasts_isPrefixOf
                @(Sum Int) @Int & property

--------------------------------------------------------------------------------
-- fromState
--------------------------------------------------------------------------------

prop_fromState_headState
    :: (Eq s, Show s) => s -> Property
prop_fromState_headState state =
    Seq.headState (Seq.fromState state) === state

prop_fromState_lastState
    :: (Eq s, Show s) => s -> Property
prop_fromState_lastState state =
    Seq.lastState (Seq.fromState state) === state

--------------------------------------------------------------------------------
-- appendMany
--------------------------------------------------------------------------------

prop_appendMany_headState
    :: (Eq s, Show s) => s -> Fun (s, d) s -> [d] -> Property
prop_appendMany_headState state nextStateFn deltas =
    Seq.headState result === state
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_appendMany_size
    :: (Eq s, Show s) => s -> Fun (s, d) s -> [d] -> Property
prop_appendMany_size state nextStateFn deltas =
    Seq.size result === length deltas
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

--------------------------------------------------------------------------------
-- dropHeads
--------------------------------------------------------------------------------

prop_dropHeads_head
    :: (Eq s, Show s, Eq d, Show d) => s -> Fun (s, d) s -> [d] -> Property
prop_dropHeads_head state nextStateFn deltas =
    NE.head (Seq.dropHeads result) === Seq.fromState (Seq.lastState result)
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_dropHeads_last
    :: (Eq s, Show s, Eq d, Show d) => s -> Fun (s, d) s -> [d] -> Property
prop_dropHeads_last state nextStateFn deltas =
    NE.last (Seq.dropHeads result) === result
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_dropHeads_length
    :: (Eq s, Show s, Eq d, Show d) => s -> Fun (s, d) s -> [d] -> Property
prop_dropHeads_length state nextStateFn deltas =
    NE.length (Seq.dropHeads result) === length deltas + 1
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_dropHeads_isSuffixOf
    :: (Eq s, Show s, Eq d) => s -> Fun (s, d) s -> [d] -> Property
prop_dropHeads_isSuffixOf state nextStateFn deltas =
    all (uncurry Seq.isSuffixOf) (consecutivePairs (Seq.dropHeads result))
        === True
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

--------------------------------------------------------------------------------
-- dropLasts
--------------------------------------------------------------------------------

prop_dropLasts_head
    :: (Eq s, Show s, Eq d, Show d) => s -> Fun (s, d) s -> [d] -> Property
prop_dropLasts_head state nextStateFn deltas =
    NE.head (Seq.dropLasts result) === Seq.fromState state
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_dropLasts_last
    :: (Eq s, Show s, Eq d, Show d) => s -> Fun (s, d) s -> [d] -> Property
prop_dropLasts_last state nextStateFn deltas =
    NE.last (Seq.dropLasts result) === result
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_dropLasts_length
    :: (Eq s, Show s, Eq d, Show d) => s -> Fun (s, d) s -> [d] -> Property
prop_dropLasts_length state nextStateFn deltas =
    NE.length (Seq.dropLasts result) === length deltas + 1
  where
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState (Seq.fromState state) deltas

prop_dropLasts_isPrefixOf
    :: (Eq s, Show s, Eq d) => s -> Fun (s, d) s -> [d] -> Property
prop_dropLasts_isPrefixOf state nextStateFn deltas =
    all (uncurry Seq.isPrefixOf) (consecutivePairs (Seq.dropLasts result))
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
