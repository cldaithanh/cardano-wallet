{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.TxSeq
    ( TxSeq
    , fromUTxO
    , appendTx
    , appendTxs
    , dropHeadTx
    , dropLastTx
    , headUTxO
    , lastUTxO
    , prefixes
    , suffixes
    , size
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

--------------------------------------------------------------------------------
-- Public interface
--------------------------------------------------------------------------------

fromUTxO :: UTxO -> TxSeq
fromUTxO = TxSeq . Seq.fromState

headUTxO :: TxSeq -> UTxO
headUTxO = Seq.headState . unTxSeq

lastUTxO :: TxSeq -> UTxO
lastUTxO = Seq.lastState . unTxSeq

appendTx :: MonadFail m => TxSeq -> Tx -> m TxSeq
appendTx s = fmap TxSeq . Seq.append (flip safeApplyTxToUTxO) (unTxSeq s)

appendTxs :: (Foldable f, MonadFail m) => TxSeq -> f Tx -> m TxSeq
appendTxs s = fmap TxSeq . Seq.appendMany (flip safeApplyTxToUTxO) (unTxSeq s)

size :: TxSeq -> Int
size = Seq.size . unTxSeq

prefixes :: TxSeq -> NonEmpty TxSeq
prefixes = fmap TxSeq . Seq.prefixes . unTxSeq

suffixes :: TxSeq -> NonEmpty TxSeq
suffixes = fmap TxSeq . Seq.suffixes . unTxSeq

dropHeadTx :: TxSeq -> Maybe TxSeq
dropHeadTx = fmap TxSeq . Seq.dropHead . unTxSeq

dropLastTx :: TxSeq -> Maybe TxSeq
dropLastTx = fmap TxSeq . Seq.dropLast . unTxSeq

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

safeApplyTxToUTxO :: MonadFail m => Tx -> UTxO -> m UTxO
safeApplyTxToUTxO tx u
    | tx `canApplyTxToUTxO` u =
        pure $ tx `applyTxToUTxO` u
    | otherwise = fail
        "cannot spend an input that does not refer to a known UTxO"
