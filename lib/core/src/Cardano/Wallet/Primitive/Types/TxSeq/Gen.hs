{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Primitive.Types.TxSeq.Gen
    (
    -- * Public interface
      ShrinkableTxSeq
    , genTxSeq
    , getTxSeq
    , shrinkTxSeq

    -- * Internal types and functions (exported for testing)
    , ShrinkState (..)
    , ShrinkPhase (..)
    , ShrinkAction (..)
    , getShrinkPhase
    , getShrinkState
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
import Data.Maybe
    ( catMaybes, listToMaybe )
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
        [ShrinkActionRemoveGroupBoundary i | i <- [1..groupBoundaryCount]]
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

initialShrinkPhase :: ShrinkPhase
initialShrinkPhase = minBound

initialShrinkState :: TxSeq -> ShrinkState
initialShrinkState = shrinkPhaseToState initialShrinkPhase

nextShrinkState :: TxSeq -> ShrinkState -> Maybe ShrinkState
nextShrinkState txSeq = \case
    ShrinkState phase (_ : actions) ->
        Just $ ShrinkState phase actions
    ShrinkState phase [] ->
        Just $ case nextShrinkPhase phase of
            Nothing -> ShrinkStateFinished
            Just sp -> shrinkPhaseToState sp txSeq
    ShrinkStateFinished ->
        Nothing

applyShrinkStateAction :: TxSeq -> ShrinkState -> [TxSeq]
applyShrinkStateAction txSeq = \case
    ShrinkState _ (action : _) ->
        applyShrinkAction action txSeq
    ShrinkState _ [] ->
        []
    ShrinkStateFinished ->
        []

getTxSeq :: ShrinkableTxSeq -> TxSeq
getTxSeq = txSeq

getShrinkPhase :: ShrinkableTxSeq -> Maybe ShrinkPhase
getShrinkPhase txSeq = case shrinkState txSeq of
    ShrinkState phase _ -> Just phase
    ShrinkStateFinished -> Nothing

getShrinkState :: ShrinkableTxSeq -> ShrinkState
getShrinkState = shrinkState

genTxSeq :: Gen UTxO -> Gen Address -> Gen ShrinkableTxSeq
genTxSeq genUTxO genAddr = fmap toShrinkable $ sized $ \size ->
    TxSeq.unfoldNM size genDelta =<< genUTxO
  where
    genDelta :: UTxO -> Gen (Either TxSeqGroupBoundary Tx)
    genDelta u = frequency
        [ (1, pure (Left TxSeqGroupBoundary))
        , (4, Right <$> genTxFromUTxO genAddr u)
        ]

    toShrinkable :: TxSeq -> ShrinkableTxSeq
    toShrinkable s = ShrinkableTxSeq (initialShrinkState s) s

shrinkTxSeq :: ShrinkableTxSeq -> [ShrinkableTxSeq]
shrinkTxSeq ShrinkableTxSeq {shrinkState, txSeq} =
    catMaybes $ toShrinkable <$>
        (applyShrinkStateAction txSeq shrinkState <> [txSeq])
  where
    toShrinkable :: TxSeq -> Maybe ShrinkableTxSeq
    toShrinkable s = flip ShrinkableTxSeq s <$> nextShrinkState s shrinkState

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
