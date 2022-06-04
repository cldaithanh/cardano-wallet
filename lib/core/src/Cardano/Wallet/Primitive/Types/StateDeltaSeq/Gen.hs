{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeq.Gen
    ( genStateDeltaSeq
    )
    where

import Prelude hiding
    ( seq )

import Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq )
import Control.Monad
    ( foldM )
import Test.QuickCheck
    ( Gen, sized )

import qualified Cardano.Wallet.Primitive.Types.StateDeltaSeq as Seq

genStateDeltaSeq
    :: forall s d. Gen s
    -> (s -> Gen d)
    -> (s -> d -> s)
    -> Gen (StateDeltaSeq s d)
genStateDeltaSeq genState genDelta nextState =
    sized $ \size -> do
        state <- genState
        foldM (const . genOne) (Seq.fromState state) (replicate size ())
  where
    genOne :: StateDeltaSeq s d -> Gen (StateDeltaSeq s d)
    genOne seq = Seq.applyDelta nextState seq <$> genDelta (Seq.lastState seq)
