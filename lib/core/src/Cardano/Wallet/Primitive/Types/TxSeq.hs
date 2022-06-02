{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.TxSeq
    ( TxSeq
    , append
    , removeTxIn
    , removeTxIns
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

import qualified Cardano.Wallet.Primitive.Types.StateDeltaSeq as Seq
import qualified Cardano.Wallet.Primitive.Types.Tx as Tx
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO

type TxSeq = StateDeltaSeq UTxO Tx

append :: MonadFail m => TxSeq -> Tx -> m TxSeq
append = Seq.append appendInner
  where
    appendInner u tx
        | tx `canApplyTxToUTxO` u =
            pure $ tx `applyTxToUTxO` u
        | otherwise =
            fail "tx spends inputs that are not present in the final UTxO"

canApplyTxToUTxO :: Tx -> UTxO -> Bool
canApplyTxToUTxO tx u =  (&&)
    (all inputIsValid (tx & Tx.resolvedInputs))
    (all inputIsValid (tx & Tx.resolvedCollateralInputs))
  where
    inputIsValid :: (TxIn, Coin) -> Bool
    inputIsValid (ti, c) = case UTxO.lookup ti u of
        Nothing -> False
        Just to -> Tx.txOutCoin to == c

removeTxIn :: TxSeq -> TxIn -> TxSeq
removeTxIn = undefined

removeTxIns :: TxSeq -> [TxSeq]
removeTxIns = undefined
