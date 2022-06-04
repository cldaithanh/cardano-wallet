{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeq.Gen
    ( ShrinkableStateDeltaSeq
    , genStateDeltaSeq
    , shrinkStateDeltaSeq
    , unwrapStateDeltaSeq
    )
    where

import Prelude hiding
    ( seq, sequence )

import Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq )
import Control.Monad
    ( foldM )
import Safe
    ( succSafe )
import Test.QuickCheck
    ( Gen, sized )

import qualified Cardano.Wallet.Primitive.Types.StateDeltaSeq as Seq
import qualified Data.List.NonEmpty as NE

data ShrinkableStateDeltaSeq s d = ShrinkableStateDeltaSeq
    { shrinkAction
        :: StateDeltaSeqShrinkAction
    , sequence
        :: StateDeltaSeq s d
    }
    deriving (Eq, Show)

unwrapStateDeltaSeq :: ShrinkableStateDeltaSeq s d -> StateDeltaSeq s d
unwrapStateDeltaSeq = sequence

genStateDeltaSeq
    :: forall s d. Gen s
    -> (s -> Gen d)
    -> (s -> d -> s)
    -> Gen (ShrinkableStateDeltaSeq s d)
genStateDeltaSeq =
    (fmap . fmap . fmap . fmap $ ShrinkableStateDeltaSeq minBound)
    (genStateDeltaSeqRaw)

genStateDeltaSeqRaw
    :: forall s d. Gen s
    -> (s -> Gen d)
    -> (s -> d -> s)
    -> Gen (StateDeltaSeq s d)
genStateDeltaSeqRaw genState genDelta nextState =
    sized $ \size -> do
        state <- genState
        foldM (const . genOne) (Seq.fromState state) (replicate size ())
  where
    genOne :: StateDeltaSeq s d -> Gen (StateDeltaSeq s d)
    genOne seq = Seq.applyDelta nextState seq <$> genDelta (Seq.lastState seq)

shrinkStateDeltaSeq
    :: ShrinkableStateDeltaSeq s d
    -> [ShrinkableStateDeltaSeq s d]
shrinkStateDeltaSeq ShrinkableStateDeltaSeq {shrinkAction, sequence} =
    ShrinkableStateDeltaSeq (succSafe shrinkAction) <$>
        applyStateDeltaSeqShrinkAction shrinkAction sequence

data StateDeltaSeqShrinkAction
    = DropHeads
    | DropLasts
    | NoShrink
    deriving (Bounded, Enum, Eq, Show)

applyStateDeltaSeqShrinkAction
    :: StateDeltaSeqShrinkAction
    -> StateDeltaSeq s d
    -> [StateDeltaSeq s d]
applyStateDeltaSeqShrinkAction = \case
    DropHeads ->
        NE.toList . Seq.dropHeads
    DropLasts ->
        NE.toList . Seq.dropLasts
    NoShrink ->
        const []
