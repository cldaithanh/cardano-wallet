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

import Prelude

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
    { availableShrinkActions
        :: [TxSeqShrinkAction]
    , txSeq
        :: TxSeq
    }
    deriving (Eq, Show)

data TxSeqShrinkAction
    = DropHeadTxs
    | DropLastTxs
    deriving (Eq, Show)

unwrapTxSeq :: ShrinkableTxSeq -> TxSeq
unwrapTxSeq = txSeq

genTxSeq :: Gen UTxO -> Gen Address -> Gen ShrinkableTxSeq
genTxSeq genUTxO genAddr = fmap makeShrinkable $ sized $ \size ->
    TxSeq.unfoldNM size (genTxFromUTxO genAddr) =<< genUTxO
  where
    makeShrinkable :: TxSeq -> ShrinkableTxSeq
    makeShrinkable txSeq = ShrinkableTxSeq
        { availableShrinkActions =
            [ DropHeadTxs
            , DropLastTxs
            ]
        , txSeq
        }

shrinkTxSeq :: ShrinkableTxSeq -> [ShrinkableTxSeq]
shrinkTxSeq ShrinkableTxSeq {availableShrinkActions, txSeq} =
    case availableShrinkActions of
        [] -> []
        shrinkAction : remainingShrinkActions ->
            ShrinkableTxSeq remainingShrinkActions <$>
                applyShrinkAction shrinkAction txSeq
  where
    applyShrinkAction :: TxSeqShrinkAction -> TxSeq -> [TxSeq]
    applyShrinkAction = \case
        DropHeadTxs ->
            NE.toList . TxSeq.dropHeadTxs
        DropLastTxs ->
            NE.toList . TxSeq.dropLastTxs

genTxFromUTxO :: Gen Address -> UTxO -> Gen Tx
genTxFromUTxO genAddr u = do
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
