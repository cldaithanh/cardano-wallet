{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Wallet.Primitive.Types.TxSeq.Gen
    ( ShrinkableTxSeq
    , genTxSeq
    , shrinkTxSeq
    , unwrapTxSeq
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Model
    ( applyTxToUTxO )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.StateDeltaSeq.Gen
    ( ShrinkableStateDeltaSeq
    , genStateDeltaSeq
    , shrinkStateDeltaSeq
    , unwrapStateDeltaSeq
    )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundlePartitionNonNull )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxOut (..) )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( TxWithoutId (..), txWithoutIdToTx )
import Cardano.Wallet.Primitive.Types.TxSeq
    ( TxSeq (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( selectUTxOEntries )
import Data.Maybe
    ( listToMaybe )
import Test.QuickCheck
    ( Gen, chooseInt, elements, vectorOf )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F

--------------------------------------------------------------------------------
-- Transaction sequences
--------------------------------------------------------------------------------

newtype ShrinkableTxSeq = ShrinkableTxSeq (ShrinkableStateDeltaSeq UTxO Tx)
    deriving (Eq, Show)

unwrapTxSeq :: ShrinkableTxSeq -> TxSeq
unwrapTxSeq (ShrinkableTxSeq s) = TxSeq (unwrapStateDeltaSeq s)

genTxSeq :: Gen UTxO -> Gen Address -> Gen ShrinkableTxSeq
genTxSeq genUTxO genAddr = ShrinkableTxSeq <$>
    genStateDeltaSeq genUTxO (`genTxFromUTxO` genAddr) (flip applyTxToUTxO)

shrinkTxSeq :: ShrinkableTxSeq -> [ShrinkableTxSeq]
shrinkTxSeq (ShrinkableTxSeq s) = ShrinkableTxSeq <$> shrinkStateDeltaSeq s

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
