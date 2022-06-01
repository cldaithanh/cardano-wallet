module Cardano.Wallet.Primitive.Types.TxSeq
    ( TxSeq
--    , appendTx
--    , fromUTxO
    , isInfixOf
    , isPrefixOf
    , isSuffixOf
    , prefixes
    , removeTxIn
    , removeTxIns
    , suffixes
    ) where

import Prelude

import Cardano.Wallet.Primitive.Model
    ( applyTxToUTxO )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx, TxIn )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )

import qualified Cardano.Wallet.Primitive.Types.Tx as Tx
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.List as L

--------------------------------------------------------------------------------
-- Concrete interface
--------------------------------------------------------------------------------

type TxSeq = TxSeqOf UTxO Tx

appendTx :: MonadFail m => TxSeq -> Tx -> m TxSeq
appendTx = appendTxWith appendInner
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

--------------------------------------------------------------------------------
-- Abstract interface
--------------------------------------------------------------------------------

data TxSeqOf state tx = TxSeq state [(tx, state)]

fromInitialState :: state -> TxSeqOf state tx
fromInitialState s0 = TxSeq s0 []

initialState :: TxSeqOf state tx -> state
initialState (TxSeq s0 _) = s0

finalState :: TxSeqOf state tx -> state
finalState s = case deltas s of
    []          -> initialState s
    (_, sf) : _ -> sf

size :: TxSeqOf state tx -> Int
size = length . deltas

deltas :: TxSeqOf state tx -> [(tx, state)]
deltas (TxSeq _ ds) = ds

appendTxWith
    :: Functor f
    => (state -> tx -> f state)
    -> TxSeqOf state tx
    -> tx
    -> f (TxSeqOf state tx)
appendTxWith nextState s t =
    appendInner <$> nextState (finalState s) t
  where
    appendInner fs = TxSeq (initialState s) ((t, fs) : deltas s)

prefixes :: TxSeqOf state tx -> [TxSeqOf state tx]
prefixes s = TxSeq (initialState s) <$> L.tails (deltas s)

suffixes :: TxSeqOf state tx -> [TxSeqOf state tx]
suffixes (TxSeq _ _) = undefined -- TxSeq u <$> L.tails ps

isPrefixOf :: (Eq state, Eq tx) => TxSeqOf state tx -> TxSeqOf state tx -> Bool
isPrefixOf s1 s2 = (&&)
    (initialState s1 == initialState s2)
    (deltas s1 `L.isPrefixOf` deltas s2)

isSuffixOf :: (Eq state, Eq tx) => TxSeqOf state tx -> TxSeqOf state tx -> Bool
isSuffixOf = undefined

isInfixOf :: (Eq state, Eq tx) => TxSeqOf state tx -> TxSeqOf state tx -> Bool
isInfixOf = undefined
