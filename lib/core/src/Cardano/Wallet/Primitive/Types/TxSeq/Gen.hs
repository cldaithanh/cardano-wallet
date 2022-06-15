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
    ( chooseCoin, genCoinPositive )
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
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes, listToMaybe )
import Safe
    ( tailMay )
import Test.QuickCheck
    ( Gen, chooseInt, elements, frequency, sized, vectorOf )
import Test.QuickCheck.Extra
    ( genMapWith )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TxSeq as TxSeq
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Transaction sequences
--------------------------------------------------------------------------------

data ShrinkableTxSeq = ShrinkableTxSeq
    { shrinkState
        :: ShrinkState
    , txSeq
        :: TxSeq
    }
    deriving (Eq, Show)

instance Ord ShrinkableTxSeq where
    compare = compare `on` show

data ShrinkState
    = ShrinkState ShrinkPhase [ShrinkAction]
    | ShrinkStateFinished
    deriving (Eq, Show)

data ShrinkPhase
    = ShrinkPhaseDropHeadTxs
    | ShrinkPhaseDropLastTxs
    | ShrinkPhaseRemoveGroupBoundaries
    | ShrinkPhaseRemoveAssetIds
    | ShrinkPhaseShrinkAssetIds
    | ShrinkPhaseShrinkTxIds
    deriving (Bounded, Enum, Eq, Ord, Show)

data ShrinkAction
    = ShrinkActionDropHeadTxs
    | ShrinkActionDropLastTxs
    | ShrinkActionRemoveGroupBoundary Int
    | ShrinkActionRemoveAssetId AssetId
    | ShrinkActionShrinkAssetIds
    | ShrinkActionShrinkTxIds
    deriving (Eq, Show)

shrinkPhaseActions :: TxSeq -> ShrinkPhase -> [ShrinkAction]
shrinkPhaseActions txSeq = \case
    ShrinkPhaseDropHeadTxs ->
        [ShrinkActionDropHeadTxs]
    ShrinkPhaseDropLastTxs ->
        [ShrinkActionDropLastTxs]
    ShrinkPhaseRemoveGroupBoundaries ->
        [ShrinkActionRemoveGroupBoundary i | i <- [1 .. groupBoundaryCount]]
    ShrinkPhaseRemoveAssetIds ->
        [ShrinkActionRemoveAssetId a | a <- assetIds]
    ShrinkPhaseShrinkAssetIds ->
        [ShrinkActionShrinkAssetIds]
    ShrinkPhaseShrinkTxIds ->
        [ShrinkActionShrinkTxIds]
  where
    assetIds = F.toList $ TxSeq.assetIds txSeq
    groupBoundaryCount = TxSeq.groupBoundaryCount txSeq

applyShrinkAction :: ShrinkAction -> TxSeq -> [TxSeq]
applyShrinkAction action txSeq = case action of
    ShrinkActionDropHeadTxs ->
        TxSeq.dropHeadTxs txSeq
    ShrinkActionDropLastTxs ->
        TxSeq.dropLastTxs txSeq
    ShrinkActionRemoveGroupBoundary _ ->
        TxSeq.removeGroupBoundary txSeq
    ShrinkActionRemoveAssetId assetId ->
        [TxSeq.removeAssetId txSeq assetId]
    ShrinkActionShrinkAssetIds ->
        [TxSeq.shrinkAssetIds txSeq]
    ShrinkActionShrinkTxIds ->
        [TxSeq.shrinkTxIds txSeq]

shrinkPhaseToState :: ShrinkPhase -> TxSeq -> ShrinkState
shrinkPhaseToState phase txSeq =
    ShrinkState phase (shrinkPhaseActions txSeq phase)

nextShrinkPhase :: ShrinkPhase -> Maybe ShrinkPhase
nextShrinkPhase = boundedEnumSucc

initialShrinkState :: TxSeq -> ShrinkState
initialShrinkState = shrinkPhaseToState minBound

nextShrinkState :: TxSeq -> ShrinkState -> Maybe ShrinkState
nextShrinkState txSeq = \case
    ShrinkStateFinished ->
        Nothing
    ShrinkState phase actions ->
        case tailMay actions of
            Just actionsRemaining ->
                Just (ShrinkState phase actionsRemaining)
            Nothing ->
                case nextShrinkPhase phase of
                    Just phaseNext ->
                        Just (shrinkPhaseToState phaseNext txSeq)
                    Nothing ->
                        Just ShrinkStateFinished

applyShrinkStateAction :: ShrinkState -> TxSeq -> [TxSeq]
applyShrinkStateAction state txSeq = case state of
    ShrinkStateFinished -> []
    ShrinkState _ actions ->
        case actions of
            [] -> [txSeq]
            (action : _) -> applyShrinkAction action txSeq

toTxSeq :: ShrinkableTxSeq -> TxSeq
toTxSeq = txSeq

toShrinkableTxSeq :: TxSeq -> ShrinkableTxSeq
toShrinkableTxSeq txSeq = ShrinkableTxSeq (initialShrinkState txSeq) txSeq

genTxSeq :: Gen UTxO -> Gen Address -> Gen ShrinkableTxSeq
genTxSeq genUTxO genAddr = fmap toShrinkableTxSeq $ sized $ \size ->
    TxSeq.unfoldNM size genDelta =<< genUTxO
  where
    genDelta :: UTxO -> Gen (Either TxSeqGroupBoundary Tx)
    genDelta u = frequency
        [ (1, pure (Left TxSeqGroupBoundary))
        , (4, Right <$> genTxFromUTxO genAddr u)
        ]

shrinkTxSeq :: ShrinkableTxSeq -> [ShrinkableTxSeq]
shrinkTxSeq ShrinkableTxSeq {shrinkState, txSeq} =
    catMaybes $ makeShrinkable <$> applyShrinkStateAction shrinkState txSeq
  where
    makeShrinkable :: TxSeq -> Maybe ShrinkableTxSeq
    makeShrinkable s = flip ShrinkableTxSeq s <$> nextShrinkState s shrinkState

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
    feeCoin <-
        min (TokenBundle.coin inputValue) <$> chooseCoin (Coin 1, Coin 4)
    let inputValueMinusFee =
            inputValue `TokenBundle.difference` TokenBundle.fromCoin feeCoin
    outputBundles <-
        genTokenBundlePartitionNonNull inputValueMinusFee =<< chooseInt (1, 3)
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
            Just feeCoin
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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

boundedEnumSucc :: (Bounded a, Enum a, Ord a) => a -> Maybe a
boundedEnumSucc a
    | a >= maxBound = Nothing
    | otherwise = Just (succ a)
