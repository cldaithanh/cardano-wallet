{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Primitive.Types.TxSeq.Gen
    ( ShrinkableTxSeq
    , genTxSeq
    , shrinkTxSeq
    , toTxSeq
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinPositive )
import Cardano.Wallet.Primitive.Types.RewardAccount.Gen
    ( genRewardAccount )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundlePartitionNonNull )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxOut (..), TxScriptValidity (..) )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( TxWithoutId (..), txWithoutIdToTx )
import Cardano.Wallet.Primitive.Types.TxSeq
    ( TxSeq (..), TxSeqGroupBoundary (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( selectUTxOEntries )
import Data.Function
    ( on )
import Data.Maybe
    ( listToMaybe )
import Test.QuickCheck
    ( Gen, chooseInt, elements, frequency, sized, vectorOf )
import Test.QuickCheck.Extra
    ( genMapWith )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TxSeq as TxSeq
import qualified Data.Foldable as F

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
    | RemoveAssetId AssetId
    | ShrinkAssetIds
    | ShrinkTxIds
    deriving (Eq, Show)

instance Ord ShrinkableTxSeq where
    compare = compare `on` show

toTxSeq :: ShrinkableTxSeq -> TxSeq
toTxSeq = txSeq

genTxSeq :: Gen UTxO -> Gen Address -> Gen ShrinkableTxSeq
genTxSeq genUTxO genAddr = fmap makeShrinkable $ sized $ \size ->
    TxSeq.unfoldNM size genDelta =<< genUTxO
  where
    genDelta :: UTxO -> Gen (Either TxSeqGroupBoundary Tx)
    genDelta u = frequency
        [ (1, pure (Left TxSeqGroupBoundary))
        , (4, Right <$> genTxFromUTxO genAddr u)
        ]

    makeShrinkable :: TxSeq -> ShrinkableTxSeq
    makeShrinkable txSeq = ShrinkableTxSeq
        { availableShrinkActions = mconcat
            [ [ DropHeadTxs
              , DropLastTxs
              , ShrinkTxIds
              ]
            , [ RemoveAssetId a | a <- F.toList (TxSeq.assetIds txSeq) ]
            , [ ShrinkAssetIds ]
            ]
        , txSeq
        }

shrinkTxSeq :: ShrinkableTxSeq -> [ShrinkableTxSeq]
shrinkTxSeq ShrinkableTxSeq {availableShrinkActions, txSeq} =
    case availableShrinkActions of
        [] -> []
        shrinkAction : remainingShrinkActions ->
            ShrinkableTxSeq remainingShrinkActions <$>
                (applyShrinkAction shrinkAction txSeq <> [txSeq])
  where
    applyShrinkAction :: TxSeqShrinkAction -> TxSeq -> [TxSeq]
    applyShrinkAction = \case
        DropHeadTxs ->
            TxSeq.dropHeadTxs
        DropLastTxs ->
            TxSeq.dropLastTxs
        RemoveAssetId asset ->
            pure . (`TxSeq.removeAssetId` asset)
        ShrinkAssetIds ->
            pure . TxSeq.shrinkAssetIds
        ShrinkTxIds ->
            pure . TxSeq.shrinkTxIds

genTxFromUTxO :: Gen Address -> UTxO -> Gen Tx
genTxFromUTxO genAddr u = do
    (inputs, _) <-
        selectUTxOEntries u =<< chooseInt (1, 2)
    (collateralInputs, _) <-
        selectUTxOEntries u =<< chooseInt (1, 2)
    withdrawals <-
        genMapWith genRewardAccount genCoinPositive
    let inputValue = mconcat
            [ F.foldMap (tokens . snd) inputs
            , F.foldMap TokenBundle.fromCoin withdrawals
            ]
    let collateralInputValue =
            F.foldMap (tokens . snd) collateralInputs
    outputBundles <-
        genTokenBundlePartitionNonNull inputValue =<< chooseInt (1, 3)
    collateralOutputBundles <-
        elements [[], [collateralInputValue]]
    outputAddresses <-
        vectorOf (length outputBundles) genAddr
    collateralOutputAddresses <-
        vectorOf (length collateralOutputBundles) genAddr
    scriptValidity <- elements
        [ Nothing
        , Just TxScriptValid
        , Just TxScriptInvalid
        ]
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
        , withdrawals
        , scriptValidity
        }
