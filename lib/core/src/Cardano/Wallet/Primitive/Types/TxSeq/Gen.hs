{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Primitive.Types.TxSeq.Gen
    ( ShrinkableTxSeq
    , genTxSeq
    , shrinkTxSeq
    , unwrapTxSeq
    )
    where

import Prelude hiding
    ( sequence )

import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
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
import Safe
    ( succSafe )
import Test.QuickCheck
    ( Gen, chooseInt, elements, sized, vectorOf )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TxSeq as TxSeq
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Transaction sequences
--------------------------------------------------------------------------------

data ShrinkableTxSeq = ShrinkableTxSeq
    { shrinkAction
        :: TxSeqShrinkAction
    , sequence
        :: TxSeq
    }
    deriving (Eq, Show)

data TxSeqShrinkAction
    = DropHeadTxs
    | DropLastTxs
    | NoShrink
    deriving (Bounded, Enum, Eq, Show)

applyTxSeqShrinkAction
    :: TxSeqShrinkAction -> TxSeq -> [TxSeq]
applyTxSeqShrinkAction = \case
    DropHeadTxs ->
        NE.toList . TxSeq.dropHeadTxs
    DropLastTxs ->
        NE.toList . TxSeq.dropLastTxs
    NoShrink ->
        const []

unwrapTxSeq :: ShrinkableTxSeq -> TxSeq
unwrapTxSeq = sequence

genTxSeq :: Gen UTxO -> Gen Address -> Gen ShrinkableTxSeq
genTxSeq = (fmap . fmap . fmap $ ShrinkableTxSeq minBound) genTxSeqRaw

genTxSeqRaw :: Gen UTxO -> Gen Address -> Gen TxSeq
genTxSeqRaw genUTxO genAddr = sized $ \size ->
    TxSeq.unfoldNM size (`genTxFromUTxO` genAddr) =<< genUTxO

shrinkTxSeq :: ShrinkableTxSeq -> [ShrinkableTxSeq]
shrinkTxSeq ShrinkableTxSeq {shrinkAction, sequence} =
    ShrinkableTxSeq (succSafe shrinkAction) <$>
    applyTxSeqShrinkAction shrinkAction sequence

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
