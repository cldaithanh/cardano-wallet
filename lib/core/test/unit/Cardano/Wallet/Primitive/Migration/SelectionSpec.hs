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
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Migration.Selection
    ( Selection (..)
    , SelectionCorrectness (..)
    , SelectionError (..)
    , SelectionFullError (..)
    , SelectionParameters (..)
    , Size (..)
    , check
    , coalesceOutputs
    , costOfOutputCoin
    , create
    , minimizeFeeForOutput
    , outputSizeWithinLimit
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, Flat (..), TokenMap )
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
import Data.Semigroup
    ( mtimesDefault )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
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
    , vector
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.Migration.SelectionSpec" $

    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Creating selections" $ do

        it "prop_create" $
            property prop_create

    parallel $ describe "Coalescing outputs" $ do

        it "prop_coalesceOutputs" $
            property prop_coalesceOutputs

    parallel $ describe "Minimizing fees" $ do

        it "prop_minimizeFeeForOutput" $
            property prop_minimizeFeeForOutput

--------------------------------------------------------------------------------
-- Creating a selection
--------------------------------------------------------------------------------

data MockCreateArguments = MockCreateArguments
    { mockSelectionParameters :: MockSelectionParameters
    , mockInputs :: NonEmpty (MockInputId, TokenBundle)
    , mockRewardWithdrawal :: Coin
    } deriving (Eq, Show)

genMockCreateArguments :: Gen MockCreateArguments
genMockCreateArguments = do
    mockSelectionParameters <- genMockSelectionParameters
    mockRewardWithdrawal <- genCoinRange (Coin 0) (Coin 100)
    inputCount <- choose (1, 10)
    mockInputs <- (:|)
        <$> genMockInput
        <*> replicateM (inputCount - 1) genMockInput
    pure MockCreateArguments
        { mockSelectionParameters
        , mockInputs
        , mockRewardWithdrawal
        }

instance Arbitrary MockCreateArguments where
    arbitrary = genMockCreateArguments

prop_create :: MockCreateArguments -> Property
prop_create args =
    checkCoverage $
    cover 50 (selectionResultIsSelection result)
        "Success" $
    cover 10 (selectionResultHasMoreInputsThanOutputs result)
        "Success with more inputs than outputs" $
    cover 10 (selectionResultHasMoreThanOneOutput result)
        "Success with more than one output" $
    cover 10 (selectionResultHasOneOutput result)
        "Success with one output" $
    cover 10 (selectionResultHasZeroFeeExcess result)
        "Success with zero fee excess" $
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
                [ check params selection === SelectionCorrect
                , inputs selection === mockInputs
                ]
  where
    MockCreateArguments
        { mockSelectionParameters
        , mockInputs
        , mockRewardWithdrawal
        } = args
    params = unMockSelectionParameters mockSelectionParameters
    result = create params mockRewardWithdrawal mockInputs

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
        <$> genTokenBundle
        <*> replicateM (mockOutputCount - 1) genTokenBundle
    pure MockCoalesceOutputsArguments
        { mockSelectionParameters
        , mockOutputs
        }

instance Arbitrary MockCoalesceOutputsArguments where
    arbitrary = genMockCoalesceOutputsArguments

prop_coalesceOutputs :: Blind MockCoalesceOutputsArguments -> Property
prop_coalesceOutputs mockArgs =
    checkCoverage $
    cover 10 (length result < length mockOutputs)
        "length result < length mockOutputs" $
    cover 2 (length result == 1)
        "length result == 1" $
    cover 2 (length result == 2)
        "length result == 2" $
    conjoin
        [ all (outputSizeWithinLimit params) result
        , F.fold result == F.fold mockOutputs
        ]
  where
    Blind MockCoalesceOutputsArguments
        { mockSelectionParameters
        , mockOutputs
        } = mockArgs
    result = coalesceOutputs params mockOutputs
    params = unMockSelectionParameters mockSelectionParameters

--------------------------------------------------------------------------------
-- Minimizing fee excesses
--------------------------------------------------------------------------------

data MockMinimizeFeeExcessForOutputArguments =
    MockMinimizeFeeExcessForOutputArguments
        { mockSelectionParameters :: MockSelectionParameters
        , mockFeeExcessToMinimize :: Coin
        , mockOutput :: TokenBundle
        }
    deriving (Eq, Show)

genMockMinimizeFeeExcessForOutputArguments
    :: Gen MockMinimizeFeeExcessForOutputArguments
genMockMinimizeFeeExcessForOutputArguments = do
    mockSelectionParameters <- genMockSelectionParameters
    mockOutput <- genTokenBundle
    mockFeeExcessToMinimize <- genCoin
    pure MockMinimizeFeeExcessForOutputArguments
        { mockSelectionParameters
        , mockFeeExcessToMinimize
        , mockOutput
        }

instance Arbitrary MockMinimizeFeeExcessForOutputArguments where
    arbitrary = genMockMinimizeFeeExcessForOutputArguments

prop_minimizeFeeForOutput
    :: Blind MockMinimizeFeeExcessForOutputArguments -> Property
prop_minimizeFeeForOutput mockArgs =
    checkCoverage $
    cover 10 (feeExcessAfter == Coin 0)
        "feeExcessAfter == 0" $
    cover 0 (feeExcessAfter /= Coin 0)
        "feeExcessAfter /= 0" $
    counterexample counterexampleText $
    -- TODO: Check that the feeExcessAfter is what is expected.
    conjoinMap
        [ ( "feeExcessAfter > feeExcessBefore"
          , feeExcessAfter <= feeExcessBefore )
        , ( "outputCoinAfter < outputCoinBefore"
          , outputCoinAfter >= outputCoinBefore )
        , ( "outputCoinCostAfter < outputCoinCostBefore"
          , outputCoinCostAfter >= outputCoinCostBefore )
        , ( "adaSpent <> feeExcessAfter /= feeExcessBefore"
          , adaSpent <> feeExcessAfter == feeExcessBefore )
        , ( "costOfIncreasingFinalOutputCoinByOne < feeExcessAfter"
          , if feeExcessAfter > Coin 0
            then costOfIncreasingFinalOutputCoinByOne >= feeExcessAfter
            else True )
        ]
  where
    Blind MockMinimizeFeeExcessForOutputArguments
        { mockSelectionParameters
        , mockFeeExcessToMinimize
        , mockOutput
        } = mockArgs

    params = unMockSelectionParameters mockSelectionParameters
    (feeExcessAfter, outputBundleAfter) =
        minimizeFeeForOutput params (mockFeeExcessToMinimize, mockOutput)

    adaSpent =
        outputCoinIncrease <> outputCostIncrease
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

genMockInput :: Gen (MockInputId, TokenBundle)
genMockInput = (,)
    <$> genMockInputId
    <*> genTokenBundle

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

genTokenBundle :: Gen TokenBundle
genTokenBundle = do
    assetCount <- oneof
        [ pure 0
        , pure 1
        , choose (2, 4)
        ]
    tokens <- TokenMap.fromFlatList <$> replicateM assetCount genAssetQuantity
    coin <- genCoin
    pure TokenBundle {coin, tokens}
  where
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
genTokenQuantity = genTokenQuantityRange (TokenQuantity 0) (TokenQuantity 1000)

genTokenQuantityRange :: TokenQuantity -> TokenQuantity -> Gen TokenQuantity
genTokenQuantityRange (TokenQuantity a) (TokenQuantity b) =
    TokenQuantity . fromIntegral @Integer
        <$> choose (fromIntegral a, fromIntegral b)

--------------------------------------------------------------------------------
-- Mock selection parameters
--------------------------------------------------------------------------------

mockSizeOfOutput :: TokenBundle -> MockSize
mockSizeOfOutput = MockSize . fromIntegral . BS.length . pretty . Flat

mockSizeOfRewardWithdrawal :: Coin -> MockSize
mockSizeOfRewardWithdrawal = \case
    Coin 0 -> MockSize 0
    Coin c -> MockSize $ fromIntegral $ BS.length $ pretty $ Coin c

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
    , mockMaximumTokenQuantity
        :: MockMaximumTokenQuantity
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
    , maximumTokenQuantity =
        unMockMaximumTokenQuantity
            $ view #mockMaximumTokenQuantity m
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
    <*> genMockMaximumTokenQuantity
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
genMockSizeOfInput = MockSizeOfInput <$> genMockSizeRange 0 10

--------------------------------------------------------------------------------
-- Mock maximum sizes of outputs
--------------------------------------------------------------------------------

data MockMaximumSizeOfOutput = MockMaximumSizeOfOutput
    { unMockMaximumSizeOfOutput :: MockSize }
    deriving (Eq, Show)

genMockMaximumSizeOfOutput :: Gen MockMaximumSizeOfOutput
genMockMaximumSizeOfOutput = MockMaximumSizeOfOutput
    -- Chosen so that the upper limit is just above the unconstrained maximum
    -- size of token bundles generated by 'genTokenBundle'.
    <$> genMockSizeRange 200 1500

--------------------------------------------------------------------------------
-- Mock maximum sizes of selections
--------------------------------------------------------------------------------

newtype MockMaximumSizeOfSelection = MockMaximumSizeOfSelection
    { unMockMaximumSizeOfSelection :: MockSize }
    deriving (Eq, Generic, Ord, Show)

genMockMaximumSizeOfSelection :: Gen MockMaximumSizeOfSelection
genMockMaximumSizeOfSelection =
    MockMaximumSizeOfSelection <$> genMockSizeRange 0 10_000

--------------------------------------------------------------------------------
-- Mock maximum token quantities
--------------------------------------------------------------------------------

newtype MockMaximumTokenQuantity = MockMaximumTokenQuantity
    { unMockMaximumTokenQuantity :: TokenQuantity }
    deriving (Eq, Generic, Ord, Show)

genMockMaximumTokenQuantity :: Gen MockMaximumTokenQuantity
genMockMaximumTokenQuantity = MockMaximumTokenQuantity <$>
    genTokenQuantityRange (TokenQuantity 100) (TokenQuantity 2000)

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

conjoinMap :: [(String, Bool)] -> Property
conjoinMap = conjoin . fmap (\(d, t) -> counterexample d t)

counterexampleMap :: [(String, String)] -> String
counterexampleMap
    = mconcat
    . fmap (\(k, v) -> k <> ":\n" <> v <> "\n\n")

matchLeft :: (e -> Bool) -> Either e a -> Bool
matchLeft f result = case result of
    Right _ -> False
    Left x -> f x

matchRight :: (a -> Bool) -> Either e a -> Bool
matchRight f result = case result of
    Right x -> f x
    Left _ -> False
