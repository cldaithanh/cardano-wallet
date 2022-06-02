{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeqSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq )
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

    describe "fromInitialState" $ do
        it "prop_fromInitialState_initialState" $
            prop_fromInitialState_initialState
                @(Sum Int) & property
        it "prop_fromInitialState_finalState" $
            prop_fromInitialState_finalState
                @(Sum Int) & property
        it "prop_fromInitialState_appendMany_initialState" $
            prop_fromInitialState_appendMany_initialState
                @(Sum Int) @Int & property
        it "prop_fromInitialState_appendMany_prefixes_head" $
            prop_fromInitialState_appendMany_prefixes_head
                @(Sum Int) @Int & property
        it "prop_fromInitialState_appendMany_prefixes_last" $
            prop_fromInitialState_appendMany_prefixes_last
                @(Sum Int) @Int & property
        it "prop_fromInitialState_appendMany_prefixes_length" $
            prop_fromInitialState_appendMany_prefixes_length
                @(Sum Int) @Int & property
        it "prop_fromInitialState_appendMany_prefixes_isPrefixOf" $
            prop_fromInitialState_appendMany_prefixes_isPrefixOf
                @(Sum Int) @Int & property
        it "prop_fromInitialState_appendMany_size" $
            prop_fromInitialState_appendMany_size
                @(Sum Int) @Int & property

prop_fromInitialState_initialState
    :: (Eq state, Show state) => state -> Property
prop_fromInitialState_initialState state =
    Seq.initialState (Seq.fromInitialState state) === state

prop_fromInitialState_finalState
    :: (Eq state, Show state) => state -> Property
prop_fromInitialState_finalState state =
    Seq.finalState (Seq.fromInitialState state) === state

prop_fromInitialState_appendMany_initialState
    :: forall state delta. (Eq state, Show state)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromInitialState_appendMany_initialState initialState nextStateFn deltas =
    Seq.initialState result === initialState
  where
    initialSeq = Seq.fromInitialState initialState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

prop_fromInitialState_appendMany_prefixes_head
    :: forall state delta. (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromInitialState_appendMany_prefixes_head
    initialState nextStateFn deltas =
        NE.head (Seq.prefixes result) === initialSeq
  where
    initialSeq = Seq.fromInitialState initialState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

prop_fromInitialState_appendMany_prefixes_last
    :: forall state delta. (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromInitialState_appendMany_prefixes_last
    initialState nextStateFn deltas =
        NE.last (Seq.prefixes result) === result
  where
    initialSeq = Seq.fromInitialState initialState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

prop_fromInitialState_appendMany_prefixes_length
    :: forall state delta. (Eq state, Show state, Eq delta, Show delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromInitialState_appendMany_prefixes_length
    initialState nextStateFn deltas =
        NE.length (Seq.prefixes result) === length deltas + 1
  where
    initialSeq = Seq.fromInitialState initialState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

prop_fromInitialState_appendMany_prefixes_isPrefixOf
    :: forall state delta. (Eq state, Show state, Eq delta)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromInitialState_appendMany_prefixes_isPrefixOf
    initialState nextStateFn deltas =
        all (uncurry Seq.isPrefixOf) (consecutivePairs (Seq.prefixes result))
            === True
  where
    initialSeq = Seq.fromInitialState initialState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

prop_fromInitialState_appendMany_size
    :: forall state delta. (Eq state, Show state)
    => state
    -> Fun (state, delta) state
    -> [delta]
    -> Property
prop_fromInitialState_appendMany_size initialState nextStateFn deltas =
    Seq.size result === length deltas
  where
    initialSeq = Seq.fromInitialState initialState
    nextState = fmap (fmap Just) (applyFun2 nextStateFn)
    Just result = Seq.appendMany nextState initialSeq deltas

consecutivePairs :: Foldable f => f a -> [(a, a)]
consecutivePairs (F.toList -> xs) = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys
