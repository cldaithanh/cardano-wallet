{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Hoist not" -}

module Cardano.Wallet.Primitive.Types.TxSeqSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId, shrinkAssetId )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TxSeq.Gen
    ( ShrinkableTxSeq, genTxSeq, getShrinkPhase, getTxSeq, shrinkTxSeq )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe, mapMaybe )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Fun (..)
    , Function (..)
    , Property
    , applyFun
    , checkCoverage
    , chooseInt
    , cover
    , forAll
    , label
    , property
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Extra
    ( genShrinkSequence, shrinkWhile )
import Test.QuickCheck.Instances.ByteString
    ()

import qualified Cardano.Wallet.Primitive.Types.TxSeq as TxSeq
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

spec :: Spec
spec = do

    describe "dropGroupBoundary" $ do
        it "prop_dropGroupBoundary_isValid" $
            prop_dropGroupBoundary_isValid & property
        it "prop_dropGroupBoundary_toTxs" $
            prop_dropGroupBoundary_toTxs & property
        it "prop_dropGroupBoundary_txGroupCount_length" $
            prop_dropGroupBoundary_txGroupCount_length & property
        it "prop_dropGroupBoundary_txGroupCount_pred" $
            prop_dropGroupBoundary_txGroupCount_pred & property

    describe "dropHeadTxs " $ do
        it "prop_dropHeadTxs_isValid" $
            prop_dropHeadTxs_isValid & property

    describe "dropLastTxs " $ do
        it "prop_dropLastTxs_isValid" $
            prop_dropLastTxs_isValid & property

    describe "genTxSeq" $ do
        it "prop_genTxSeq_isValid" $
            prop_genTxSeq_isValid & property
        it "prop_genTxSeq_toTxGroups_length" $
            prop_genTxSeq_toTxGroups_length & property
        it "prop_genTxSeq_toTxGroups_lengths" $
            prop_genTxSeq_toTxGroups_lengths & property

    describe "mapAssetIds" $ do
        it "prop_mapAssetIds_composition" $
            prop_mapAssetIds_composition
                & withMaxSuccess 20
                & property
        it "prop_mapAssetIds_identity" $
            prop_mapAssetIds_identity & property

    describe "mapTxIds" $ do
        it "prop_mapTxIds_composition" $
            prop_mapTxIds_composition
                & withMaxSuccess 20
                & property
        it "prop_mapTxIds_identity" $
            prop_mapTxIds_identity & property

    describe "shrinkAssetIds " $ do
        it "prop_shrinkAssetIds_idempotent" $
            prop_shrinkAssetIds_idempotent & property
        it "prop_shrinkAssetIds_length" $
            prop_shrinkAssetIds_length & property
        it "prop_shrinkAssetIds_isValid" $
            prop_shrinkAssetIds_isValid & property

    describe "shrinkTxIds " $ do
        it "prop_shrinkTxIds_idempotent" $
            prop_shrinkTxIds_idempotent & property
        it "prop_shrinkTxIds_length" $
            prop_shrinkTxIds_length & property
        it "prop_shrinkTxIds_isValid" $
            prop_shrinkTxIds_isValid & property

    describe "shrinkTxSeq" $ do
        it "prop_shrinkTxSeq_length" $
            prop_shrinkTxSeq_length & property
        it "prop_shrinkTxSeq_genShrinkSequence_isValid" $
            prop_shrinkTxSeq_genShrinkSequence_isValid & property
        it "prop_shrinkTxSeq_genShrinkSequence_length" $
            prop_shrinkTxSeq_genShrinkSequence_length & property
        it "prop_shrinkTxSeq_genShrinkSequence_shrinkPhases" $
            prop_shrinkTxSeq_genShrinkSequence_shrinkPhases & property
        it "prop_shrinkTxSeq_minimum_length" $
            prop_shrinkTxSeq_minimum_length & property

    describe "toTxs" $ do
        it "prop_toTxs_txCount" $
            prop_toTxs_txCount & property

    describe "toTxGroups" $ do
        it "prop_toTxGroups_txGroupCount" $
            prop_toTxGroups_txGroupCount & property
        it "prop_toTxGroups_toTxs" $
            prop_toTxGroups_toTxs & property

--------------------------------------------------------------------------------
-- dropGroupBoundary
--------------------------------------------------------------------------------

prop_dropGroupBoundary_isValid :: ShrinkableTxSeq -> Property
prop_dropGroupBoundary_isValid (getTxSeq -> txSeq) =
    all TxSeq.isValid (TxSeq.dropGroupBoundary txSeq) === True

prop_dropGroupBoundary_toTxs :: ShrinkableTxSeq -> Property
prop_dropGroupBoundary_toTxs (getTxSeq -> txSeq) =
    all (== TxSeq.toTxs txSeq) (TxSeq.toTxs <$> TxSeq.dropGroupBoundary txSeq)
    === True

prop_dropGroupBoundary_txGroupCount_length
    :: ShrinkableTxSeq -> Property
prop_dropGroupBoundary_txGroupCount_length (getTxSeq -> txSeq) =
     length (TxSeq.dropGroupBoundary txSeq)
        === pred (TxSeq.txGroupCount txSeq)

prop_dropGroupBoundary_txGroupCount_pred
    :: ShrinkableTxSeq -> Property
prop_dropGroupBoundary_txGroupCount_pred (getTxSeq -> txSeq)
    | txGroupCount == 0 =
        TxSeq.dropGroupBoundary txSeq === []
    | otherwise =
        all (== pred txGroupCount)
            (TxSeq.txGroupCount <$> TxSeq.dropGroupBoundary txSeq)
        === True
  where
    txGroupCount = TxSeq.txGroupCount txSeq

--------------------------------------------------------------------------------
-- dropHeadTxs
--------------------------------------------------------------------------------

prop_dropHeadTxs_isValid :: ShrinkableTxSeq -> Property
prop_dropHeadTxs_isValid (getTxSeq -> txs) =
    all TxSeq.isValid (TxSeq.dropHeadTxs txs) === True

--------------------------------------------------------------------------------
-- dropLastTxs
--------------------------------------------------------------------------------

prop_dropLastTxs_isValid :: ShrinkableTxSeq -> Property
prop_dropLastTxs_isValid (getTxSeq -> txs) =
    all TxSeq.isValid (TxSeq.dropLastTxs txs) === True

--------------------------------------------------------------------------------
-- genTxSeq
--------------------------------------------------------------------------------

prop_genTxSeq_isValid :: Property
prop_genTxSeq_isValid =
    forAll (genTxSeq genUTxO genAddress) $ \(getTxSeq -> txs) ->
        TxSeq.isValid txs

prop_genTxSeq_toTxGroups_length :: Property
prop_genTxSeq_toTxGroups_length =
    forAll (genTxSeq genUTxO genAddress) $ \(getTxSeq -> txSeq) ->
        let txGroups = TxSeq.toTxGroups txSeq in
        checkCoverage
            $ cover 1 (length txGroups == 1)
                "number of groups = 1"
            $ cover 10 (length txGroups > 1)
                "number of groups > 1"
            $ property True

prop_genTxSeq_toTxGroups_lengths :: Property
prop_genTxSeq_toTxGroups_lengths =
    forAll (genTxSeq genUTxO genAddress) $ \(getTxSeq -> txSeq) ->
        let txGroups = TxSeq.toTxGroups txSeq in
        checkCoverage
            $ cover 5 (null (NE.head txGroups))
                "number of elements in head group = 0"
            $ cover 5 (length (NE.head txGroups) == 1)
                "number of elements in head group = 1"
            $ cover 5 (length (NE.head txGroups) > 1)
                "number of elements in head group > 1"
            $ cover 5 (null (NE.last txGroups))
                "number of elements in last group = 0"
            $ cover 5 (length (NE.last txGroups) == 1)
                "number of elements in last group = 1"
            $ cover 5 (length (NE.last txGroups) > 1)
                "number of elements in last group > 1"
            $ property True

--------------------------------------------------------------------------------
-- mapAssetIds
--------------------------------------------------------------------------------

prop_mapAssetIds_composition
    :: ShrinkableTxSeq
    -> Fun AssetId AssetId
    -> Fun AssetId AssetId
    -> Property
prop_mapAssetIds_composition
    (getTxSeq -> txs) (applyFun -> f) (applyFun -> g) =
        TxSeq.mapAssetIds f (TxSeq.mapAssetIds g txs) ===
        TxSeq.mapAssetIds (f . g) txs

prop_mapAssetIds_identity :: ShrinkableTxSeq -> Property
prop_mapAssetIds_identity (getTxSeq -> txs) =
    TxSeq.mapAssetIds id txs === txs

--------------------------------------------------------------------------------
-- mapTxIds
--------------------------------------------------------------------------------

prop_mapTxIds_composition
    :: ShrinkableTxSeq
    -> Fun (Hash "Tx") (Hash "Tx")
    -> Fun (Hash "Tx") (Hash "Tx")
    -> Property
prop_mapTxIds_composition
    (getTxSeq -> txs) (applyFun -> f) (applyFun -> g) =
        TxSeq.mapTxIds f (TxSeq.mapTxIds g txs) ===
        TxSeq.mapTxIds (f . g) txs

prop_mapTxIds_identity :: ShrinkableTxSeq -> Property
prop_mapTxIds_identity (getTxSeq -> txs) =
    TxSeq.mapTxIds id txs === txs

--------------------------------------------------------------------------------
-- shrinkAssetIds
--------------------------------------------------------------------------------

prop_shrinkAssetIds_idempotent :: ShrinkableTxSeq -> Property
prop_shrinkAssetIds_idempotent (getTxSeq -> txs) =
    TxSeq.assetIds (f (f txs)) === TxSeq.assetIds (f txs)
  where
    f = TxSeq.shrinkAssetIds

prop_shrinkAssetIds_length :: ShrinkableTxSeq -> Property
prop_shrinkAssetIds_length (getTxSeq -> txs) =
    length (TxSeq.assetIds (f txs)) === length (TxSeq.assetIds txs)
  where
    f = TxSeq.shrinkAssetIds

prop_shrinkAssetIds_isValid :: ShrinkableTxSeq -> Property
prop_shrinkAssetIds_isValid (getTxSeq -> txs) =
    TxSeq.isValid (TxSeq.shrinkAssetIds txs) === True

--------------------------------------------------------------------------------
-- shrinkTxIds
--------------------------------------------------------------------------------

prop_shrinkTxIds_idempotent :: ShrinkableTxSeq -> Property
prop_shrinkTxIds_idempotent (getTxSeq -> txs) =
    TxSeq.txIds (f (f txs)) === TxSeq.txIds (f txs)
  where
    f = TxSeq.shrinkTxIds

prop_shrinkTxIds_length :: ShrinkableTxSeq -> Property
prop_shrinkTxIds_length (getTxSeq -> txs) =
    length (TxSeq.txIds (f txs)) === length (TxSeq.txIds txs)
  where
    f = TxSeq.shrinkTxIds

prop_shrinkTxIds_isValid :: ShrinkableTxSeq -> Property
prop_shrinkTxIds_isValid (getTxSeq -> txs) =
    TxSeq.isValid (TxSeq.shrinkTxIds txs) === True

--------------------------------------------------------------------------------
-- shrinkTxSeq
--------------------------------------------------------------------------------

prop_shrinkTxSeq_length :: Property
prop_shrinkTxSeq_length =
    forAll (genTxSeq genUTxO genAddress) $ \txs ->
    forAll (chooseInt (0, TxSeq.length (getTxSeq txs))) $ \targetLength ->
    prop_inner txs targetLength
  where
    prop_inner :: ShrinkableTxSeq -> Int -> Property
    prop_inner txs targetLength =
        TxSeq.length (getTxSeq $ shrinkTxSeqToLength targetLength txs)
            === targetLength

    shrinkTxSeqToLength :: Int -> ShrinkableTxSeq -> ShrinkableTxSeq
    shrinkTxSeqToLength targetLength txs = fromMaybe txs $
        shrinkWhile
        ((>= targetLength) . TxSeq.length . getTxSeq)
        shrinkTxSeq
        txs

prop_shrinkTxSeq_genShrinkSequence_isValid :: Property
prop_shrinkTxSeq_genShrinkSequence_isValid =
    forAll (genShrinkSequence shrinkTxSeq =<< genTxSeq genUTxO genAddress) $
        \txSeqs ->
            all TxSeq.isValid (getTxSeq <$> txSeqs)

prop_shrinkTxSeq_genShrinkSequence_length :: Property
prop_shrinkTxSeq_genShrinkSequence_length =
    forAll (genShrinkSequence shrinkTxSeq =<< genTxSeq genUTxO genAddress) $
        \txSeqs ->
            label (
                "sequence length: " <>
                showSizeRange (sizeRange (length txSeqs))
                )
            True
  where
    showSizeRange (sizeRangeMin, sizeRangeMax) = unwords
        [ show sizeRangeMin
        , "-"
        , show sizeRangeMax
        ]

    sizeRange i = (sizeRangeMin, sizeRangeMax)
      where
        sizeRangeMin = (i `div` 10) * 10
        sizeRangeMax = sizeRangeMin + 9

prop_shrinkTxSeq_genShrinkSequence_shrinkPhases :: Property
prop_shrinkTxSeq_genShrinkSequence_shrinkPhases =
    forAll (genShrinkSequence shrinkTxSeq =<< genTxSeq genUTxO genAddress) $
        \txSeqs ->
            Set.fromList (mapMaybe getShrinkPhase txSeqs) ===
            Set.fromList [minBound .. maxBound]

prop_shrinkTxSeq_minimum_length :: Property
prop_shrinkTxSeq_minimum_length =
    forAll (genTxSeq genUTxO genAddress) $ \s0 ->
        case shrinkSpaceMinimum shrinkTxSeq s0 of
            Nothing -> TxSeq.length (getTxSeq s0) === 0
            Just s1 -> TxSeq.length (getTxSeq s1) === 0
  where
    shrinkSpaceMinimum :: (a -> [a]) -> a -> Maybe a
    shrinkSpaceMinimum = shrinkWhile (const True)

--------------------------------------------------------------------------------
-- toTxs
--------------------------------------------------------------------------------

prop_toTxs_txCount :: ShrinkableTxSeq -> Property
prop_toTxs_txCount (getTxSeq -> txSeq) =
    length (TxSeq.toTxs txSeq) === TxSeq.txCount txSeq

--------------------------------------------------------------------------------
-- toTxGroups
--------------------------------------------------------------------------------

prop_toTxGroups_txGroupCount :: ShrinkableTxSeq -> Property
prop_toTxGroups_txGroupCount (getTxSeq -> txSeq) =
    length (TxSeq.toTxGroups txSeq) === TxSeq.txGroupCount txSeq

prop_toTxGroups_toTxs :: ShrinkableTxSeq -> Property
prop_toTxGroups_toTxs (getTxSeq -> txSeq) =
    F.fold (TxSeq.toTxGroups txSeq) === TxSeq.toTxs txSeq

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary AssetId where
    arbitrary = genAssetId
    shrink = shrinkAssetId

deriving newtype instance Arbitrary (Hash "Tx")

instance Arbitrary ShrinkableTxSeq where
    arbitrary = genTxSeq genUTxO genAddress
    shrink = shrinkTxSeq

deriving anyclass instance CoArbitrary (Hash "TokenPolicy")
deriving anyclass instance CoArbitrary (Hash "Tx")
deriving anyclass instance CoArbitrary AssetId
deriving anyclass instance CoArbitrary TokenName
deriving anyclass instance CoArbitrary TokenPolicyId

deriving anyclass instance Function (Hash "TokenPolicy")
deriving anyclass instance Function (Hash "Tx")
deriving anyclass instance Function AssetId
deriving anyclass instance Function TokenName
deriving anyclass instance Function TokenPolicyId
