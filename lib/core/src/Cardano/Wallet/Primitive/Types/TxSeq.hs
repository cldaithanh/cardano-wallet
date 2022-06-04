{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.TxSeq
    ( TxSeq
    , fromUTxO
    , appendTxM
    , appendTxsM
    , dropHeadTx
    , dropHeadTxs
    , dropLastTx
    , dropLastTxs
    , headUTxO
    , lastUTxO
    , size
    , toTxs
    , isValid
    ) where

import Prelude hiding
    ( seq )

import Cardano.Wallet.Primitive.Model
    ( applyTxToUTxO )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx, TxIn )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty )

import qualified Cardano.Wallet.Primitive.Types.StateDeltaSeq as Seq
import qualified Cardano.Wallet.Primitive.Types.Tx as Tx
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO

newtype TxSeq = TxSeq {unTxSeq :: StateDeltaSeq UTxO Tx}
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Public interface
--------------------------------------------------------------------------------

fromUTxO :: UTxO -> TxSeq
fromUTxO = TxSeq . Seq.fromState

headUTxO :: TxSeq -> UTxO
headUTxO = Seq.headState . unTxSeq

lastUTxO :: TxSeq -> UTxO
lastUTxO = Seq.lastState . unTxSeq

appendTxM :: MonadFail m => TxSeq -> Tx -> m TxSeq
appendTxM = (fmap TxSeq .) . Seq.applyDeltaM safeAppendTx . unTxSeq

appendTxsM :: (Foldable f, MonadFail m) => TxSeq -> f Tx -> m TxSeq
appendTxsM = (fmap TxSeq .) . Seq.applyDeltasM safeAppendTx . unTxSeq

toTxs :: TxSeq -> [Tx]
toTxs = Seq.toDeltaList . unTxSeq

size :: TxSeq -> Int
size = Seq.size . unTxSeq

dropHeadTx :: TxSeq -> Maybe TxSeq
dropHeadTx = fmap TxSeq . Seq.dropHead . unTxSeq

dropHeadTxs :: TxSeq -> NonEmpty TxSeq
dropHeadTxs = fmap TxSeq . Seq.dropHeads . unTxSeq

dropLastTx :: TxSeq -> Maybe TxSeq
dropLastTx = fmap TxSeq . Seq.dropLast . unTxSeq

dropLastTxs :: TxSeq -> NonEmpty TxSeq
dropLastTxs = fmap TxSeq . Seq.dropLasts . unTxSeq

isValid :: TxSeq -> Bool
isValid = (Just True ==) . Seq.isValidM safeAppendTx . unTxSeq

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

canApplyTxToUTxO :: Tx -> UTxO -> Bool
canApplyTxToUTxO tx u =  (&&)
    (all inputRefIsValid (tx & Tx.resolvedInputs))
    (all inputRefIsValid (tx & Tx.resolvedCollateralInputs))
  where
    inputRefIsValid :: (TxIn, Coin) -> Bool
    inputRefIsValid (ti, c) = case UTxO.lookup ti u of
        Nothing -> False
        Just to -> Tx.txOutCoin to == c

safeAppendTx :: MonadFail m => UTxO -> Tx -> m UTxO
safeAppendTx = flip safeApplyTxToUTxO

safeApplyTxToUTxO :: MonadFail m => Tx -> UTxO -> m UTxO
safeApplyTxToUTxO tx u
    | tx `canApplyTxToUTxO` u =
        pure $ tx `applyTxToUTxO` u
    | otherwise = fail
        "cannot spend an input that does not refer to a known UTxO"
