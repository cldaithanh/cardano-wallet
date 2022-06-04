{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeqSpec
    ( spec
    ) where

import Prelude hiding
    ( seq )

import Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq )
import Data.Function
    ( (&) )
import Data.Monoid
    ( Sum )
import GHC.Generics
    ( Generic )
import Safe
    ( tailMay )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Fun
    , Function (..)
    , Property
    , applyArbitrary3
    , applyFun2
    , genericShrink
    , property
    , (===)
    )

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

    describe "applyDeltas" $ do
        it "prop_applyDeltas_headState" $
            prop_applyDeltas_headState
                @(Sum Int) @Int & property
        it "prop_applyDeltas_size" $
            prop_applyDeltas_size
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
        it "prop_dropHeads_isValid" $
            prop_dropHeads_isValid
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
        it "prop_dropLasts_isValid" $
            prop_dropLasts_isValid
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
-- applyDeltasM
--------------------------------------------------------------------------------

prop_applyDeltas_headState
    :: (Eq s, Show s) => s -> Fun (s, d) s -> [d] -> Property
prop_applyDeltas_headState state nextStateFn deltas =
    Seq.headState result === state
  where
    nextState = applyFun2 nextStateFn
    result = Seq.applyDeltas nextState (Seq.fromState state) deltas

prop_applyDeltas_size
    :: (Eq s, Show s) => s -> Fun (s, d) s -> [d] -> Property
prop_applyDeltas_size state nextStateFn deltas =
    Seq.size result === length deltas
  where
    nextState = applyFun2 nextStateFn
    result = Seq.applyDeltas nextState (Seq.fromState state) deltas

--------------------------------------------------------------------------------
-- dropHeads
--------------------------------------------------------------------------------

prop_dropHeads_head
    :: (Eq s, Eq d, Show s, Show d) => GenStateDeltaSeq s d -> Property
prop_dropHeads_head (genStateDeltaSeq -> (seq, _nextState)) =
    NE.head (Seq.dropHeads seq) === Seq.fromState (Seq.lastState seq)

prop_dropHeads_last
    :: (Eq s, Show s, Eq d, Show d) => GenStateDeltaSeq s d -> Property
prop_dropHeads_last (genStateDeltaSeq -> (seq, _nextState)) =
    NE.last (Seq.dropHeads seq) === seq

prop_dropHeads_length
    :: (Eq s, Eq d) => GenStateDeltaSeq s d -> Property
prop_dropHeads_length (genStateDeltaSeq -> (seq, _nextState)) =
    NE.length (Seq.dropHeads seq) === Seq.size seq + 1

prop_dropHeads_isSuffixOf
    :: (Eq s, Eq d) => GenStateDeltaSeq s d -> Property
prop_dropHeads_isSuffixOf (genStateDeltaSeq -> (seq, _nextState)) =
    all (uncurry Seq.isSuffixOf) (consecutivePairs (Seq.dropHeads seq)) === True

prop_dropHeads_isValid
    :: (Eq s, Eq d) => GenStateDeltaSeq s d -> Property
prop_dropHeads_isValid (genStateDeltaSeq -> (seq, nextState)) =
    all (Seq.isValid nextState) (Seq.dropHeads seq) === True

--------------------------------------------------------------------------------
-- dropLasts
--------------------------------------------------------------------------------

prop_dropLasts_head
    :: (Eq s, Eq d, Show s, Show d) => GenStateDeltaSeq s d -> Property
prop_dropLasts_head (genStateDeltaSeq -> (seq, _nextState)) =
    NE.head (Seq.dropLasts seq) === Seq.fromState (Seq.headState seq)

prop_dropLasts_last
    :: (Eq s, Show s, Eq d, Show d) => GenStateDeltaSeq s d -> Property
prop_dropLasts_last (genStateDeltaSeq -> (seq, _nextState)) =
    NE.last (Seq.dropLasts seq) === seq

prop_dropLasts_length
    :: (Eq s, Eq d) => GenStateDeltaSeq s d -> Property
prop_dropLasts_length (genStateDeltaSeq -> (seq, _nextState)) =
    NE.length (Seq.dropLasts seq) === Seq.size seq + 1

prop_dropLasts_isPrefixOf
    :: (Eq s, Eq d) => GenStateDeltaSeq s d -> Property
prop_dropLasts_isPrefixOf (genStateDeltaSeq -> (seq, _nextState)) =
    all (uncurry Seq.isPrefixOf) (consecutivePairs (Seq.dropLasts seq)) === True

prop_dropLasts_isValid
    :: (Eq s, Eq d) => GenStateDeltaSeq s d -> Property
prop_dropLasts_isValid (genStateDeltaSeq -> (seq, nextState)) =
    all (Seq.isValid nextState) (Seq.dropLasts seq) === True

--------------------------------------------------------------------------------
-- Utility types and functions
--------------------------------------------------------------------------------

data GenStateDeltaSeq s d = GenStateDeltaSeq
    { state :: s
    , deltas :: [d]
    , nextStateFn :: Fun (s, d) s
    }
    deriving (Generic, Show)

genStateDeltaSeq
    :: GenStateDeltaSeq s d -> (StateDeltaSeq s d, (s -> d -> s))
genStateDeltaSeq GenStateDeltaSeq {state, deltas, nextStateFn} =
    (seq, nextState)
  where
    nextState = applyFun2 nextStateFn
    seq = Seq.applyDeltas nextState (Seq.fromState state) deltas

deriving instance (Eq s, Eq d, Eq (Fun (s, d) s)) => Eq (GenStateDeltaSeq s d)

instance
    ( Arbitrary d
    , Arbitrary s
    , CoArbitrary d
    , CoArbitrary s
    , Function d
    , Function s
    ) =>
    Arbitrary (GenStateDeltaSeq s d)
  where
    arbitrary = applyArbitrary3 GenStateDeltaSeq
    shrink = genericShrink

consecutivePairs :: Foldable f => f a -> [(a, a)]
consecutivePairs (F.toList -> xs) = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys
