{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Wallet.Primitive.Types.TxSeq.Gen
    where

import Prelude

import Cardano.Wallet.Primitive.Model
    ( applyTxToUTxO )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundlePartitionNonNull )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxOut (..) )
import Cardano.Wallet.Primitive.Types.TxSeq
    ( TxSeq )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( TxWithoutId (..)
    , txWithoutIdToTx
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( selectUTxOEntries )
import Control.Monad
    ( foldM )
import Data.Bifunctor
    ( first )
import Data.Maybe
    ( listToMaybe )
import Test.QuickCheck
    ( Gen
    , chooseInt
    , elements
    , sized
    , vectorOf
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TxSeq as TxSeq
import qualified Data.Foldable as F

--------------------------------------------------------------------------------
-- Transaction sequences
--------------------------------------------------------------------------------

genTxSeq :: UTxO -> Gen Address -> Gen TxSeq
genTxSeq u genAddr = do
    (txs, _) <- genTxsFromUTxO u genAddr
    case TxSeq.fromUTxO u `TxSeq.appendTxs` txs of
        Nothing -> error "Unable to construct tx sequence"
        Just s -> pure s

genTxFromUTxO :: UTxO -> Gen Address -> Gen Tx
genTxFromUTxO u genAddr = do
    (inputs, _) <-
        selectUTxOEntries u =<< chooseInt (1, 4)
    (collateralInputs, _) <-
        selectUTxOEntries u =<< chooseInt (1, 2)
    let inputValue =
            F.foldMap (tokens . snd) inputs
    let collateralInputValue =
            F.foldMap (tokens . snd) collateralInputs
    outputBundles <-
        genTokenBundlePartitionNonNull inputValue =<< chooseInt (1, 4)
    collateralOutputBundles <-
        elements [[], [collateralInputValue]]
    outputAddresses <-
        vectorOf (length outputBundles) genAddr
    collateralOutputAddresses <-
        vectorOf (length collateralOutputBundles) genAddr
    pure $ txWithoutIdToTx TxWithoutId
        { fee =
            Just (Coin 0)
        , resolvedInputs =
            fmap (TokenBundle.getCoin . tokens) <$> inputs
        , resolvedCollateralInputs =
            fmap (TokenBundle.getCoin . tokens) <$> collateralInputs
        , outputs =
            zipWith TxOut outputAddresses outputBundles
        , collateralOutput = listToMaybe $
            zipWith TxOut collateralOutputAddresses collateralOutputBundles
        , metadata =
            Nothing
        , withdrawals =
            mempty
        , scriptValidity =
            Nothing
        }

genTxsFromUTxO :: UTxO -> Gen Address -> Gen ([Tx], UTxO)
genTxsFromUTxO u0 genAddr = sized $ \txCount ->
    first reverse <$> foldM (const . genOne) ([], u0) (replicate txCount ())
  where
    genOne :: ([Tx], UTxO) -> Gen ([Tx], UTxO)
    genOne (txs, u) = do
        tx <- genTxFromUTxO u genAddr
        pure (tx : txs, applyTxToUTxO tx u)

{-
-- move this to separate module.

data TxSeq = TxSeq UTxO [(Tx, UTxO)]

instance ShrinkState TxSeqShrinkState TxSeq where
    shrinkInit _ = TxSeqShrinkStateUnshrunk
    shrinkState txSeq = \case
        TxSeqShrinkStateUnshrunk ->
            (, TxSeqShrinkStatePrefix) <$> txSeqPrefixes txSeq
        TxSeqShrinkStatePrefix ->
            (, TxSeqShrinkStateSuffix) <$> txSeqSuffixes txSeq
        TxSeqShrinkStateSuffix ->
            []

-- Can try to remove suffix
-- Can try to remove prefix
--
data TxSeqShrinkState
    = TxSeqShrinkStateUnshrunk
    | TxSeqShrinkStatePrefix
    | TxSeqShrinkStateSuffix
-}
