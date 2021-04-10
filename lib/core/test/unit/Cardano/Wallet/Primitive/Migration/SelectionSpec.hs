{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Migration.SelectionSpec
    where

-- TODO:
-- Add Quiet Show instances
--
import Prelude

import Fmt
    ( pretty )
import Cardano.Wallet.Primitive.Migration.Selection
    ( AddEntry
    , ReclaimAdaResult (..)
    , excessAdaForOutput
    , reclaimAda
    , minimizeFeeExcess
    , Size (..)
    , Selection (..)
    , SelectionError (..)
    , SelectionFullError (..)
    , SelectionInvariantStatus (..)
    , SelectionOutputSizeAssessment (..)
    , SelectionOutputSizeAssessor (..)
    , SelectionParameters (..)
    --, addBundleAsNewOutput
    , addBundleAsNewOutputWithoutReclaimingAda
    , addBundleToExistingOutput
    , checkInvariant
    , coalesceOutputs
    , costOfOutputCoin
    , initialize
    , outputSatisfiesMinimumAdaQuantity
    , outputSizeWithinLimit
    , outputOrdering
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap, Flat (..) )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdLargeRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Control.Monad
    ( replicateM )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( isRight )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Maybe
    ( isJust, isNothing )
import Data.Semigroup
    ( mtimesDefault )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it)
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , Property
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , genericShrink
    , oneof
    , property
    , suchThat
    , suchThatMap
    , vector
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.Migration.SelectionSpec" $

    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Initializing a selection" $ do

        it "prop_initialize" $
            property prop_initialize

    parallel $ describe "Extending a selection" $ do

        it "prop_addBundleToExistingOutput" $
            property prop_addBundleToExistingOutput
        it "prop_addBundleAsNewOutputWithoutReclaimingAda" $
            property prop_addBundleAsNewOutputWithoutReclaimingAda

    parallel $ describe "Coalescing token bundles" $ do

        it "prop_coalesceOutputs" $
            property prop_coalesceOutputs

    parallel $ describe "Reclaiming ada from outputs" $ do

        it "prop_reclaimAda" $
            property prop_reclaimAda

    parallel $ describe "Minimizing fee excesses" $ do

        it "prop_minimizeFeeExcess" $
            property prop_minimizeFeeExcess

--------------------------------------------------------------------------------
-- Initializing a selection
--------------------------------------------------------------------------------

data MockInitializeArguments = MockInitializeArguments
    { mockSelectionParameters :: MockSelectionParameters
    , mockInputs :: NonEmpty (MockInputId, TokenBundle)
    , mockRewardWithdrawal :: Coin
    } deriving (Eq, Show)

genMockInitializeArguments :: Gen MockInitializeArguments
genMockInitializeArguments = do
    mockSelectionParameters <- genMockSelectionParameters
    mockRewardWithdrawal <- genCoin
    inputCount <- choose (1, 10)
    mockInputs <- (:|)
        <$> genMockInput mockSelectionParameters
        <*> replicateM
            (inputCount - 1)
            (genMockInput mockSelectionParameters)
    pure MockInitializeArguments
        { mockSelectionParameters
        , mockInputs
        , mockRewardWithdrawal
        }

instance Arbitrary MockInitializeArguments where
    arbitrary = genMockInitializeArguments

prop_initialize :: MockInitializeArguments -> Property
prop_initialize args =
    checkCoverage $
    cover 50 (selectionResultIsSelection result)
        "Success" $
    cover 10 (selectionResultHasMoreInputsThanOutputs result)
        "Success with more inputs than outputs" $
    cover 10 (selectionResultHasMoreThanOneOutput result)
        "Success with more than one output" $
    cover 10 (selectionResultHasOneOutput result)
        "Success with one output" $
    cover 10 (selectionResultHasNonZeroFeeExcess result)
        "Success with non-zero fee excess" $
    cover 5 (selectionResultHasInsufficientAda result)
        "Failure due to insufficient ada" $
    cover 5 (selectionResultIsFull result)
        "Failure due to oversized selection" $
    case result of
        Left SelectionAdaInsufficient ->
            -- TODO: Check that the ada amount really is insufficient.
            property True
        Left (SelectionFull e) ->
            property (selectionSizeMaximum e < selectionSizeRequired e)
        Right selection ->
            conjoin
                [ checkInvariant params selection === SelectionInvariantHolds
                , inputs selection === mockInputs
                ]
  where
    MockInitializeArguments
        { mockSelectionParameters
        , mockInputs
        , mockRewardWithdrawal
        } = args
    params = unMockSelectionParameters mockSelectionParameters
    result = initialize params mockRewardWithdrawal mockInputs

--------------------------------------------------------------------------------
-- Extending a selection
--------------------------------------------------------------------------------

data MockAddEntryArguments = MockAddEntryArguments
    { mockSelectionParameters :: MockSelectionParameters
    , mockSelection :: MockSelection
    , mockEntry :: (MockInputId, TokenBundle)
    }
    deriving (Eq, Show)

genMockAddEntryArguments :: Gen MockAddEntryArguments
genMockAddEntryArguments = flip suchThatMap eitherToMaybe $ do
    MockInitializeArguments
        { mockSelectionParameters
        , mockInputs
        , mockRewardWithdrawal
        } <- genMockInitializeArguments
    let params = unMockSelectionParameters mockSelectionParameters
    case initialize params mockRewardWithdrawal mockInputs of
        Left e ->
            pure $ Left e
        Right mockSelection -> do
            mockEntry <- (,)
                <$> genMockInputId
                <*> genTokenBundle mockSelectionParameters
            pure $ Right MockAddEntryArguments
                { mockSelectionParameters
                , mockSelection
                , mockEntry
                }

instance Arbitrary MockAddEntryArguments where
    arbitrary = genMockAddEntryArguments

type MockAddEntry v = AddEntry MockSize MockInputId v

prop_addBundleToExistingOutput :: MockAddEntryArguments -> Property
prop_addBundleToExistingOutput mockArgs =
    prop_addEntry mockArgs addBundleToExistingOutput

prop_addBundleAsNewOutputWithoutReclaimingAda
    :: MockAddEntryArguments -> Property
prop_addBundleAsNewOutputWithoutReclaimingAda mockArgs =
    prop_addEntry mockArgs addBundleAsNewOutputWithoutReclaimingAda

-- TODO: think of a way to extract out the specific properties we need for
-- specific functions.

prop_addEntry :: MockAddEntryArguments -> MockAddEntry TokenBundle -> Property
prop_addEntry mockArgs addEntry =
    checkCoverage $
    cover 30 (selectionResultIsSelection result)
        "Success" $
    cover 0.5 (selectionResultHasInsufficientAda result)
        "Failure due to insufficient ada" $
    cover 0.2 (selectionResultIsFull result)
        "Failure due to oversized selection" $
    case result of
        Left (SelectionFull e) ->
            counterexample "Failure due to oversized selection" $
            conjoin
                [ property (selectionSizeMaximum e < selectionSizeRequired e)
                --, property (isLeft initializeResult)
                ]
        Left SelectionAdaInsufficient ->
            counterexample "Failure due to insufficient ada" $
            property True -- property (isLeft initializeResult)
        Right selection ->
            counterexample "Succeeded" $
            conjoin
                [ checkInvariant params selection === SelectionInvariantHolds
                , inputs selection === mockEntry `NE.cons` inputs mockSelection
                ]
  where
    MockAddEntryArguments
        { mockSelectionParameters
        , mockSelection
        , mockEntry
        } = mockArgs
    params = unMockSelectionParameters mockSelectionParameters
    result = addEntry params mockSelection mockEntry

    --initializeResult = initialize params
      --  (rewardWithdrawal mockSelection)
       -- (mockEntry `NE.cons` inputs mockSelection)

--------------------------------------------------------------------------------
-- Coalescing token bundles
--------------------------------------------------------------------------------

data MockCoalesceOutputsArguments = MockCoalesceOutputsArguments
    { mockSelectionParameters :: MockSelectionParameters
    , mockOutputs :: NonEmpty TokenBundle
    }
    deriving (Eq, Show)

genMockCoalesceOutputsArguments :: Gen MockCoalesceOutputsArguments
genMockCoalesceOutputsArguments = do
    mockSelectionParameters <- genMockSelectionParameters
    mockOutputCount <- choose (1, 10)
    mockOutputs <- (:|)
        <$> genTokenBundle mockSelectionParameters
        <*> replicateM
            (mockOutputCount - 1)
            (genTokenBundle mockSelectionParameters)
    pure MockCoalesceOutputsArguments
        { mockSelectionParameters
        , mockOutputs
        }

instance Arbitrary MockCoalesceOutputsArguments where
    arbitrary = genMockCoalesceOutputsArguments

prop_coalesceOutputs :: Blind MockCoalesceOutputsArguments -> Property
prop_coalesceOutputs mockArgs =
    checkCoverage $
    cover 50 (length result < length mockOutputs)
        "length result < length mockOutputs" $
    cover 10 (length result == 1)
        "length result == 1" $
    cover 10 (length result == 2)
        "length result == 2" $
    cover 10 (length result == 3)
        "length result == 3" $
    cover 10 (length result >= 4)
        "length result >= 4" $
    conjoin
        [ property $ length result <= length mockOutputs
        , property $ all (outputSizeWithinLimit params) result
        , F.fold result === F.fold mockOutputs
        ]
  where
    Blind MockCoalesceOutputsArguments
        { mockSelectionParameters
        , mockOutputs
        } = mockArgs
    result = coalesceOutputs params mockOutputs
    params = unMockSelectionParameters mockSelectionParameters

--------------------------------------------------------------------------------
-- Reclaiming ada from outputs
--------------------------------------------------------------------------------

data MockReclaimAdaArguments = MockReclaimAdaArguments
    { mockSelectionParameters :: MockSelectionParameters
    , mockAdaToReclaim :: Coin
    , mockOutputs :: NonEmpty TokenBundle
    }
    deriving (Eq, Show)

genMockReclaimAdaArguments :: Gen MockReclaimAdaArguments
genMockReclaimAdaArguments = do
    mockSelectionParameters <- genMockSelectionParameters
    let params = unMockSelectionParameters mockSelectionParameters
    mockOutputCount <- choose (1, 10)
    let genOutput = genTokenBundle mockSelectionParameters `suchThat`
            outputSatisfiesMinimumAdaQuantity params
    mockOutputs <- (:|)
        <$> genOutput
        <*> replicateM (mockOutputCount - 1) genOutput
    mockAdaToReclaim <-
        -- Specially chosen to give a success rate of approximately 50%:
        genCoinRange (Coin 0) (Coin 5000)
    pure MockReclaimAdaArguments
        { mockSelectionParameters
        , mockAdaToReclaim
        , mockOutputs
        }

instance Arbitrary MockReclaimAdaArguments where
    arbitrary = genMockReclaimAdaArguments

prop_reclaimAda :: Blind MockReclaimAdaArguments -> Property
prop_reclaimAda mockArgs =
    checkCoverage $
    cover 30 (resultIsSuccess result)
        "Success" $
    cover 0.5 (resultHasZeroCostReduction result)
        "Success with zero cost reduction" $
    cover 0.5 (resultHasZeroSizeReduction result)
        "Success with zero size reduction" $
    cover 20 (resultHasNonZeroCostReduction result)
        "Success with non-zero cost reduction" $
    cover 20 (resultHasNonZeroSizeReduction result)
        "Success with non-zero size reduction" $
    cover 30 (resultIsFailure result)
        "Failure to reclaim ada" $
    case result of
        Nothing ->
            propFailure
        Just successfulResult ->
            propSuccess successfulResult
  where
    propFailure :: Property
    propFailure
        = counterexample counterexampleText
        $ counterexample "Failure to reclaim ada"
        $ property $ excessAda < mockAdaToReclaim
      where
        counterexampleText = counterexampleMap
            [ ( "outputs"
              , unlines (pretty . Flat <$> NE.toList mockOutputs) )
            , ( "ada amounts"
              , unlines (show <$> NE.toList adaAmounts) )
            , ( "excess ada amounts"
              , unlines (show <$> NE.toList excessAdaAmounts) )
            , ( "adaToReclaim"
              , show mockAdaToReclaim )
            , ( "excessAda"
              , show excessAda )
            ]
        excessAda = F.fold excessAdaAmounts
        excessAdaAmounts = excessAdaForOutput params <$> mockOutputs
        adaAmounts = view #coin <$> mockOutputs

    propSuccess :: ReclaimAdaResult MockSize -> Property
    propSuccess successfulResult = counterexample counterexampleText $ conjoin
        [ counterexample "costReduction /= costReductionExpected" $
            costReduction === costReductionExpected
        , counterexample "sizeReduction /= sizeReductionExpected" $
            sizeReduction === sizeReductionExpected
        , counterexample "tokenBalanceAfter /= tokenBalanceBefore" $
            tokenBalanceAfter === tokenBalanceBefore
        , counterexample "lengthAfter /= lengthBefore" $
            lengthAfter === lengthBefore
        , counterexample "sort (reducedOutputs) /= reducedOutputs" $
            NE.sortBy (outputOrdering params) reducedOutputs === reducedOutputs
        , counterexample "zeroness of cost and size reduction disagree" $
            (sizeReduction == mempty) === (costReduction == mempty)
        , counterexample "adaReclaimed < adaToReclaim" $
            property $ adaReclaimed >= mockAdaToReclaim

        ]
      where
        counterexampleText = counterexampleMap
            [ ( "tokenBalanceBefore"
              , pretty (Flat tokenBalanceBefore) )
            , ( "tokenBalanceAfter"
              , pretty (Flat tokenBalanceAfter) )
            , ( "costReduction"
              , show costReduction )
            , ( "costReductionExpected"
              , show costReductionExpected )
            , ( "sizeReduction"
              , show sizeReduction )
            , ( "sizeReductionExpected"
              , show sizeReductionExpected )
            , ( "reductionInOutputAda"
              , show reductionInOutputAda )
            , ( "adaReclaimed"
              , show adaReclaimed )
            , ( "adaToReclaim"
              , show mockAdaToReclaim )
            ]

        ReclaimAdaResult
            {reducedOutputs, costReduction, sizeReduction} = successfulResult
        costReductionExpected = Coin.distance
            (F.foldMap (costOfOutput params) mockOutputs)
            (F.foldMap (costOfOutput params) reducedOutputs)
        sizeReductionExpected = sizeDistance
            (F.foldMap (sizeOfOutput params) mockOutputs)
            (F.foldMap (sizeOfOutput params) reducedOutputs)
        reductionInOutputAda = Coin.distance
            (F.foldMap (view #coin) mockOutputs)
            (F.foldMap (view #coin) reducedOutputs)
        tokenBalanceAfter =
             F.foldMap (view #tokens) reducedOutputs
        tokenBalanceBefore =
            F.foldMap (view #tokens) mockOutputs
        lengthAfter =
            F.length reducedOutputs
        lengthBefore =
            F.length mockOutputs
        adaReclaimed =
            reductionInOutputAda <> costReduction

    params = unMockSelectionParameters mockSelectionParameters

    Blind MockReclaimAdaArguments
        { mockSelectionParameters
        , mockAdaToReclaim
        , mockOutputs
        } = mockArgs

    result = reclaimAda params mockAdaToReclaim mockOutputs

    resultIsSuccess = isJust

    resultHasZeroCostReduction = matchJust $ \r ->
        costReduction r == mempty

    resultHasZeroSizeReduction = matchJust $ \r ->
        sizeReduction r == mempty

    resultHasNonZeroCostReduction = matchJust $ \r ->
        costReduction r > mempty

    resultHasNonZeroSizeReduction = matchJust $ \r ->
        sizeReduction r > mempty

    resultIsFailure = isNothing

--------------------------------------------------------------------------------
-- Minimizing fee excesses
--------------------------------------------------------------------------------

data MockMinimizeFeeExcessArguments = MockMinimizeFeeExcessArguments
    { mockSelectionParameters :: MockSelectionParameters
    , mockFeeExcessToMinimize :: Coin
    , mockOutput :: TokenBundle
    }
    deriving (Eq, Show)

genMockMinimizeFeeExcessArguments :: Gen MockMinimizeFeeExcessArguments
genMockMinimizeFeeExcessArguments = do
    mockSelectionParameters <- genMockSelectionParameters
    mockOutput <- genTokenBundle mockSelectionParameters
    mockFeeExcessToMinimize <- genCoin
    pure MockMinimizeFeeExcessArguments
        { mockSelectionParameters
        , mockFeeExcessToMinimize
        , mockOutput
        }

instance Arbitrary MockMinimizeFeeExcessArguments where
    arbitrary = genMockMinimizeFeeExcessArguments

conjoinMap :: [(String, Bool)] -> Property
conjoinMap = conjoin . fmap (\(d, t) -> counterexample d t)

prop_minimizeFeeExcess :: Blind MockMinimizeFeeExcessArguments -> Property
prop_minimizeFeeExcess mockArgs =
    checkCoverage $
    cover 10 (feeExcessAfter == Coin 0)
        "feeExcessAfter == 0" $
    cover 0.05 (feeExcessAfter /= Coin 0)
        "feeExcessAfter /= 0" $
    counterexample counterexampleText $
    -- TODO: Check that the feeExcessAfter is what is expected.
    conjoinMap
        [ ("feeExcessAfter >  feeExcessBefore"
          , feeExcessAfter <= feeExcessBefore)
        , ("outputCoinAfter <  outputCoinBefore"
          , outputCoinAfter >= outputCoinBefore)
        , ("outputCoinCostAfter <  outputCoinCostBefore"
          , outputCoinCostAfter >= outputCoinCostBefore)
        , ("outputCoinIncrease <> outputCostIncrease <> feeExcessAfter /= feeExcessBefore"
          , outputCoinIncrease <> outputCostIncrease <> feeExcessAfter == feeExcessBefore)
        , ("feeExcessAfter > Coin 0 && costOfIncreasingFinalOutputCoinByOne < feeExcessAfter"
          , if feeExcessAfter > Coin 0
            then costOfIncreasingFinalOutputCoinByOne >= feeExcessAfter
            else True)
        ]
  where
    Blind MockMinimizeFeeExcessArguments
        { mockSelectionParameters
        , mockFeeExcessToMinimize
        , mockOutput
        } = mockArgs

    params = unMockSelectionParameters mockSelectionParameters
    (feeExcessAfter, outputBundleAfter) =
        minimizeFeeExcess params (mockFeeExcessToMinimize, mockOutput)

    feeExcessBefore =
        mockFeeExcessToMinimize
    outputCoinBefore =
        view #coin mockOutput
    outputCoinAfter =
        view #coin outputBundleAfter
    outputCoinIncrease =
        Coin.distance outputCoinBefore outputCoinAfter
    outputCoinCostBefore =
        costOfOutputCoin params outputCoinBefore
    outputCoinCostAfter =
        costOfOutputCoin params outputCoinAfter
    outputCostIncrease =
        Coin.distance outputCoinCostBefore outputCoinCostAfter
    outputMinimumAdaQuantity =
        minimumAdaQuantityForOutput params (view #tokens mockOutput)
    costOfIncreasingFinalOutputCoinByOne =
        Coin.distance
            (costOfOutputCoin params outputCoinAfter)
            (costOfOutputCoin params (outputCoinAfter <> Coin 1))

    counterexampleText = counterexampleMap
        [ ( "feeExcessBefore"
          , show feeExcessBefore )
        , ( "feeExcessAfter"
          , show feeExcessAfter )
        , ( "outputMinimumAdaQuantity"
          , show outputMinimumAdaQuantity )
        , ( "outputCoinBefore"
          , show outputCoinBefore )
        , ( "outputCoinAfter"
          , show outputCoinAfter )
        , ( "outputCoinCostBefore"
          , show outputCoinCostBefore )
        , ( "outputCoinCostAfter"
          , show outputCoinCostAfter )
        , ( "outputCostIncrease"
          , show outputCostIncrease )
        , ( "costOfIncreasingFinalOutputCoinByOne"
          , show costOfIncreasingFinalOutputCoinByOne )
        ]

--------------------------------------------------------------------------------
-- Mock results
--------------------------------------------------------------------------------

type MockSelection = Selection MockInputId MockSize
type MockSelectionError = SelectionError MockSize
type MockSelectionResult = Either MockSelectionError MockSelection

selectionResultIsSelection :: MockSelectionResult -> Bool
selectionResultIsSelection = isRight

selectionResultHasMoreInputsThanOutputs :: MockSelectionResult -> Bool
selectionResultHasMoreInputsThanOutputs = matchRight $ \selection ->
    F.length (inputs selection) > F.length (outputs selection)

selectionResultHasMoreThanOneOutput :: MockSelectionResult -> Bool
selectionResultHasMoreThanOneOutput = matchRight $ \selection ->
    F.length (outputs selection) > 1

selectionResultHasOneOutput :: MockSelectionResult -> Bool
selectionResultHasOneOutput = matchRight $ \selection ->
    F.length (outputs selection) == 1

selectionResultHasNonZeroFeeExcess :: MockSelectionResult -> Bool
selectionResultHasNonZeroFeeExcess = matchRight $ \selection ->
    feeExcess selection > Coin 0

selectionResultHasZeroFeeExcess :: MockSelectionResult -> Bool
selectionResultHasZeroFeeExcess = matchRight $ \selection ->
    feeExcess selection == Coin 0

selectionResultHasInsufficientAda :: MockSelectionResult -> Bool
selectionResultHasInsufficientAda = matchLeft $ \case
    SelectionAdaInsufficient -> True
    _ -> False

selectionResultIsFull :: MockSelectionResult -> Bool
selectionResultIsFull = matchLeft $ \case
    SelectionFull _ -> True
    _ -> False

--------------------------------------------------------------------------------
-- Generating inputs
--------------------------------------------------------------------------------

genMockInput :: MockSelectionParameters -> Gen (MockInputId, TokenBundle)
genMockInput mockParams = (,)
    <$> genMockInputId
    <*> genTokenBundle mockParams

--------------------------------------------------------------------------------
-- Generating input identifiers
--------------------------------------------------------------------------------

newtype MockInputId = MockInputId
    { unMockInputId :: ByteString }
    deriving (Eq, Ord)

instance Show MockInputId where
    show = T.unpack . T.decodeUtf8 . convertToBase Base16 . unMockInputId

genMockInputId :: Gen MockInputId
genMockInputId = MockInputId . BS.pack <$> vector 8

--------------------------------------------------------------------------------
-- Generating token bundles
--------------------------------------------------------------------------------

genTokenBundle :: MockSelectionParameters -> Gen TokenBundle
genTokenBundle mockParams =
    genInner  `suchThat` outputSizeWithinLimit params
  where
    params = unMockSelectionParameters mockParams

    genInner = do
        assetCount <- oneof
            [ pure 0
            , pure 1
            , choose (2, 4)
            ]
        tokens <- TokenMap.fromFlatList <$>
            replicateM assetCount genAssetQuantity
        coin <- genCoin
        pure TokenBundle {coin, tokens}

    genAssetQuantity :: Gen (AssetId, TokenQuantity)
    genAssetQuantity = (,)
        <$> genAssetIdLargeRange
        <*> genTokenQuantity

--------------------------------------------------------------------------------
-- Generating coins
--------------------------------------------------------------------------------

genCoin :: Gen Coin
genCoin = genCoinRange (Coin 1) (Coin 1000)

genCoinRange :: Coin -> Coin -> Gen Coin
genCoinRange (Coin minCoin) (Coin maxCoin) =
    Coin . fromIntegral <$> choose (minCoin, maxCoin)

--------------------------------------------------------------------------------
-- Generating token quantities
--------------------------------------------------------------------------------

genTokenQuantity :: Gen TokenQuantity
genTokenQuantity = TokenQuantity . fromIntegral @Integer <$> choose (1, 1000)

--------------------------------------------------------------------------------
-- Generating selections
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Mock selection parameters
--------------------------------------------------------------------------------

mockSizeOfOutput :: TokenBundle -> MockSize
mockSizeOfOutput = MockSize . fromIntegral . length . show

mockSizeOfRewardWithdrawal :: Coin -> MockSize
mockSizeOfRewardWithdrawal = \case
    Coin 0 -> MockSize 0
    Coin c -> MockSize $ fromIntegral $ length $ show $ Coin c

mockSizeToFee :: MockSize -> Coin
mockSizeToFee = Coin . fromIntegral . unMockSize

data MockSelectionParameters = MockSelectionParameters
    { mockFeeForEmptySelection
        :: MockFeeForEmptySelection
    , mockSizeOfEmptySelection
        :: MockSizeOfEmptySelection
    , mockSizeOfInput
        :: MockSizeOfInput
    , mockMaximumSizeOfOutput
        :: MockMaximumSizeOfOutput
    , mockMaximumSizeOfSelection
        :: MockMaximumSizeOfSelection
    , mockMinimumAdaQuantityForOutput
        :: MockMinimumAdaQuantityForOutput
    }
    deriving (Eq, Generic, Show)

unMockSelectionParameters
    :: MockSelectionParameters -> SelectionParameters MockSize
unMockSelectionParameters m = SelectionParameters
    { costOfEmptySelection =
        unMockFeeForEmptySelection
            $ view #mockFeeForEmptySelection m
    , costOfInput =
        mockSizeToFee <$> unMockSizeOfInput
            $ view #mockSizeOfInput m
    , costOfOutput =
        mockSizeToFee . mockSizeOfOutput
    , costOfRewardWithdrawal =
        mockSizeToFee . mockSizeOfRewardWithdrawal
    , sizeOfEmptySelection =
        unMockSizeOfEmptySelection
            $ view #mockSizeOfEmptySelection m
    , sizeOfInput =
        unMockSizeOfInput
            $ view #mockSizeOfInput m
    , sizeOfOutput =
        mockSizeOfOutput
    , sizeOfRewardWithdrawal =
        mockSizeOfRewardWithdrawal
    , maximumSizeOfOutput =
        unMockMaximumSizeOfOutput
            $ view #mockMaximumSizeOfOutput m
    , maximumSizeOfSelection =
        unMockMaximumSizeOfSelection
            $ view #mockMaximumSizeOfSelection m
    , minimumAdaQuantityForOutput =
        unMockMinimumAdaQuantityForOutput
            $ view #mockMinimumAdaQuantityForOutput m
    }

genMockSelectionParameters :: Gen MockSelectionParameters
genMockSelectionParameters = MockSelectionParameters
    <$> genMockFeeForEmptySelection
    <*> genMockSizeOfEmptySelection
    <*> genMockSizeOfInput
    <*> genMockMaximumSizeOfOutput
    <*> genMockMaximumSizeOfSelection
    <*> genMockMinimumAdaQuantityForOutput

instance Arbitrary MockSelectionParameters where
    arbitrary = genMockSelectionParameters

--------------------------------------------------------------------------------
-- Mock fees for empty selections
--------------------------------------------------------------------------------

newtype MockFeeForEmptySelection = MockFeeForEmptySelection
    { unMockFeeForEmptySelection :: Coin }
    deriving stock Eq
    deriving Show via Coin

genMockFeeForEmptySelection :: Gen MockFeeForEmptySelection
genMockFeeForEmptySelection = MockFeeForEmptySelection
    <$> genCoinRange (Coin 0) (Coin 100)

--------------------------------------------------------------------------------
-- Mock sizes
--------------------------------------------------------------------------------

newtype MockSize = MockSize { unMockSize :: Natural }
    deriving stock (Eq, Ord)
    deriving Show via Natural

instance Semigroup MockSize where
    MockSize a <> MockSize b = MockSize (a + b)

instance Monoid MockSize where
    mempty = MockSize 0

instance Size MockSize where
    MockSize a `sizeDistance` MockSize b
        | a >= b    = MockSize (a - b)
        | otherwise = MockSize (b - a)

genMockSizeRange :: Natural -> Natural -> Gen MockSize
genMockSizeRange minSize maxSize =
    MockSize . fromIntegral @Integer @Natural <$>
        choose (fromIntegral minSize, fromIntegral maxSize)

mockSizeSubtractSafe :: MockSize -> MockSize -> MockSize
mockSizeSubtractSafe (MockSize a) (MockSize b)
    | a >= b = MockSize (a - b)
    | otherwise = MockSize 0

--------------------------------------------------------------------------------
-- Mock sizes of empty selections
--------------------------------------------------------------------------------

newtype MockSizeOfEmptySelection = MockSizeOfEmptySelection
    { unMockSizeOfEmptySelection :: MockSize }
    deriving (Eq, Generic, Ord, Show)

genMockSizeOfEmptySelection :: Gen MockSizeOfEmptySelection
genMockSizeOfEmptySelection =
    MockSizeOfEmptySelection <$> genMockSizeRange 0 100

--------------------------------------------------------------------------------
-- Mock sizes of inputs
--------------------------------------------------------------------------------

newtype MockSizeOfInput = MockSizeOfInput
    { unMockSizeOfInput :: MockSize }
    deriving stock Eq
    deriving Show via Natural

genMockSizeOfInput :: Gen MockSizeOfInput
genMockSizeOfInput =
    MockSizeOfInput <$> genMockSizeRange 0 10

--------------------------------------------------------------------------------
-- Mock maximum sizes of outputs
--------------------------------------------------------------------------------

data MockMaximumSizeOfOutput = MockMaximumSizeOfOutput
    { mockMaximumOutputSize
        :: Maybe MockSize
    }
    deriving (Eq, Show)

noMaximumOutputSize :: MockMaximumSizeOfOutput
noMaximumOutputSize = MockMaximumSizeOfOutput Nothing

unMockMaximumSizeOfOutput
    :: MockMaximumSizeOfOutput -> SelectionOutputSizeAssessor
unMockMaximumSizeOfOutput mock = SelectionOutputSizeAssessor assess
  where
    assess bundle
        | Just limit <- mockMaximumOutputSize mock
        , mockSizeOfOutput bundle <= limit =
            SelectionOutputSizeWithinLimit
        | otherwise =
            SelectionOutputSizeExceedsLimit

genMockMaximumSizeOfOutput :: Gen MockMaximumSizeOfOutput
genMockMaximumSizeOfOutput = MockMaximumSizeOfOutput
    -- Chosen so that the upper limit is just above the unconstrained maximum
    -- size of token bundles generated by 'genTokenBundle'.
    <$> (Just <$> genMockSizeRange 100 1500)

--------------------------------------------------------------------------------
-- Mock maximum sizes of selections
--------------------------------------------------------------------------------

newtype MockMaximumSizeOfSelection = MockMaximumSizeOfSelection
    { unMockMaximumSizeOfSelection :: MockSize }
    deriving (Eq, Generic, Ord, Show)

genMockMaximumSizeOfSelection :: Gen MockMaximumSizeOfSelection
genMockMaximumSizeOfSelection =
    MockMaximumSizeOfSelection <$> genMockSizeRange 0 10000

--------------------------------------------------------------------------------
-- Mock minimum ada quantities for outputs
--------------------------------------------------------------------------------

data MockMinimumAdaQuantityForOutput = MockMinimumAdaQuantityForOutput
    { mockMinimumAdaQuantityPerOutput :: Coin
    , mockMinimumAdaQuantityPerOutputAsset :: Coin
    }
    deriving (Eq, Show)

unMockMinimumAdaQuantityForOutput
    :: MockMinimumAdaQuantityForOutput
    -> (TokenMap -> Coin)
unMockMinimumAdaQuantityForOutput mock = \m ->
    let assetCount = Set.size $ TokenMap.getAssets m in
    mockMinimumAdaQuantityPerOutput mock
        <> mtimesDefault assetCount (mockMinimumAdaQuantityPerOutputAsset mock)

genMockMinimumAdaQuantityForOutput :: Gen MockMinimumAdaQuantityForOutput
genMockMinimumAdaQuantityForOutput = MockMinimumAdaQuantityForOutput
    <$> genCoinRange (Coin 0) (Coin 10)
    <*> genCoinRange (Coin 0) (Coin 10)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

--------------------------------------------------------------------------------
-- Internal types and functions
--------------------------------------------------------------------------------

counterexampleMap :: [(String, String)] -> String
counterexampleMap
    = mconcat
    . fmap (\(k, v) -> k <> ":\n" <> v <> "\n\n")

matchJust :: (a -> Bool) -> Maybe a -> Bool
matchJust f result = case result of
    Nothing -> False
    Just x -> f x

matchLeft :: (e -> Bool) -> Either e a -> Bool
matchLeft f result = case result of
    Right _ -> False
    Left x -> f x

matchRight :: (a -> Bool) -> Either e a -> Bool
matchRight f result = case result of
    Right x -> f x
    Left _ -> False
