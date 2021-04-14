{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Migration.SelectionSpec
    where

import Prelude

import Cardano.Wallet.Primitive.Migration.Selection
    ( Selection (..)
    , SelectionCorrectness (..)
    , SelectionError (..)
    , SelectionFullError (..)
    , TxSize (..)
    , check
    , coalesceOutputs
    , create
    , minimizeFee
    , minimizeFeeForOutput
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( Flat (..), TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdLargeRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxConstraints (..)
    , txOutputCoinCost
    , txOutputCoinSize
    , txOutputHasValidSize
    )
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
import Data.Maybe
    ( fromMaybe )
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
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
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

        it "prop_minimizeFee" $
            property prop_minimizeFee
        it "prop_minimizeFeeForOutput" $
            property prop_minimizeFeeForOutput

    parallel $ describe "Cost calculations" $ do

        it "prop_txOutputCost" $
            property prop_txOutputCost

    parallel $ describe "Size calculations" $ do

        it "prop_txOutputSize" $
            property prop_txOutputSize

--------------------------------------------------------------------------------
-- Creating a selection
--------------------------------------------------------------------------------

type MockSelection = Selection MockInputId MockSize
type MockSelectionError = SelectionError MockSize
type MockSelectionResult = Either MockSelectionError MockSelection

data MockCreateArguments = MockCreateArguments
    { mockConstraints :: MockTxConstraints
    , mockInputs :: NonEmpty (MockInputId, TokenBundle)
    , mockRewardWithdrawal :: Coin
    } deriving (Eq, Show)

genMockCreateArguments :: Gen MockCreateArguments
genMockCreateArguments = do
    mockConstraints <- genMockTxConstraints
    mockRewardWithdrawal <- oneof
        [ pure (Coin 0)
        , genCoinRange (Coin 1) (Coin 1_000_000)
        ]
    inputCount <- choose (1, 10)
    mockInputs <- (:|)
        <$> genMockInput
        <*> replicateM (inputCount - 1) genMockInput
    pure MockCreateArguments
        { mockConstraints
        , mockInputs
        , mockRewardWithdrawal
        }

instance Arbitrary MockCreateArguments where
    arbitrary = genMockCreateArguments

prop_create :: MockCreateArguments -> Property
prop_create args =
    checkCoverage $
    cover 40 (resultIsSelection result)
        "Success" $
    cover 10 (resultHasMoreInputsThanOutputs result)
        "Success with more inputs than outputs" $
    cover 10 (resultHasMoreThanOneOutput result)
        "Success with more than one output" $
    cover 10 (resultHasOneOutput result)
        "Success with one output" $
    cover 10 (resultHasZeroFeeExcess result)
        "Success with zero fee excess" $
    cover 5 (resultHasInsufficientAda result)
        "Failure due to insufficient ada" $
    cover 5 (resultIsFull result)
        "Failure due to oversized selection" $
    case result of
        Left SelectionAdaInsufficient ->
            -- TODO: Check that the ada amount really is insufficient.
            property True
        Left (SelectionFull e) ->
            property (selectionSizeMaximum e < selectionSizeRequired e)
        Right selection ->
            conjoin
                [ check constraints selection === SelectionCorrect
                , inputs selection === mockInputs
                ]
  where
    MockCreateArguments
        { mockConstraints
        , mockInputs
        , mockRewardWithdrawal
        } = args
    constraints = unMockTxConstraints mockConstraints
    result = create constraints mockRewardWithdrawal mockInputs

    resultIsSelection :: MockSelectionResult -> Bool
    resultIsSelection = isRight

    resultHasMoreInputsThanOutputs :: MockSelectionResult -> Bool
    resultHasMoreInputsThanOutputs = matchRight $ \selection ->
        F.length (inputs selection) > F.length (outputs selection)

    resultHasMoreThanOneOutput :: MockSelectionResult -> Bool
    resultHasMoreThanOneOutput = matchRight $ \selection ->
        F.length (outputs selection) > 1

    resultHasOneOutput :: MockSelectionResult -> Bool
    resultHasOneOutput = matchRight $ \selection ->
        F.length (outputs selection) == 1

    resultHasZeroFeeExcess :: MockSelectionResult -> Bool
    resultHasZeroFeeExcess = matchRight $ \selection ->
        feeExcess selection == Coin 0

    resultHasInsufficientAda :: MockSelectionResult -> Bool
    resultHasInsufficientAda = matchLeft $ \case
        SelectionAdaInsufficient -> True
        _ -> False

    resultIsFull :: MockSelectionResult -> Bool
    resultIsFull = matchLeft $ \case
        SelectionFull _ -> True
        _ -> False

--------------------------------------------------------------------------------
-- Coalescing outputs
--------------------------------------------------------------------------------

data MockCoalesceOutputsArguments = MockCoalesceOutputsArguments
    { mockConstraints :: MockTxConstraints
    , mockOutputs :: NonEmpty TokenBundle
    }
    deriving (Eq, Show)

genMockCoalesceOutputsArguments :: Gen MockCoalesceOutputsArguments
genMockCoalesceOutputsArguments = do
    mockConstraints <- genMockTxConstraints
    mockOutputCount <- choose (1, 10)
    mockOutputs <- (:|)
        <$> genTokenBundle
        <*> replicateM (mockOutputCount - 1) genTokenBundle
    pure MockCoalesceOutputsArguments
        { mockConstraints
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
        [ all (txOutputHasValidSize constraints) result
        , F.fold result == F.fold mockOutputs
        ]
  where
    Blind MockCoalesceOutputsArguments
        { mockConstraints
        , mockOutputs
        } = mockArgs
    result = coalesceOutputs constraints mockOutputs
    constraints = unMockTxConstraints mockConstraints

--------------------------------------------------------------------------------
-- Minimizing fees
--------------------------------------------------------------------------------

data MockMinimizeFeeArguments = MockMinimizeFeeArguments
    { mockConstraints :: MockTxConstraints
    , mockFeeExcessToMinimize :: Coin
    , mockOutputs :: NonEmpty TokenBundle
    }
    deriving (Eq, Show)

genMockMinimizeFeeArguments :: Gen MockMinimizeFeeArguments
genMockMinimizeFeeArguments = do
    mockConstraints <- genMockTxConstraints
    mockOutputCount <- choose (1, 10)
    mockOutputs <- (:|)
        <$> genTokenBundle
        <*> replicateM (mockOutputCount - 1) genTokenBundle
    mockFeeExcessToMinimize <- genCoinRange (Coin 0) (Coin 10_000)
    pure MockMinimizeFeeArguments
        { mockConstraints
        , mockFeeExcessToMinimize
        , mockOutputs
        }

instance Arbitrary MockMinimizeFeeArguments where
    arbitrary = genMockMinimizeFeeArguments

prop_minimizeFee :: Blind MockMinimizeFeeArguments -> Property
prop_minimizeFee mockArgs =
    checkCoverage $
    cover 50 (feeExcessAfter == Coin 0)
        "feeExcessAfter == 0" $
    cover 50 (totalOutputCostIncrease > Coin 0)
        "totalOutputCostIncrease > 0" $
    conjoin $
        [ length outputsAfter == length outputsBefore
        , feeExcessAfter <= feeExcessBefore
        , totalOutputCostIncrease <> totalOutputAdaIncrease ==
            feeExcessReduction
        ]
  where
    Blind MockMinimizeFeeArguments
        { mockConstraints
        , mockFeeExcessToMinimize
        , mockOutputs
        } = mockArgs
    constraints = unMockTxConstraints mockConstraints

    (feeExcessAfter, outputsAfter) =
        minimizeFee constraints (mockFeeExcessToMinimize, mockOutputs)
    feeExcessBefore =
        mockFeeExcessToMinimize
    feeExcessReduction =
        Coin.distance feeExcessBefore feeExcessAfter
    outputsBefore =
        mockOutputs

    totalOutputAdaAfter =
        F.foldMap (view #coin) outputsAfter
    totalOutputAdaBefore =
        F.foldMap (view #coin) outputsBefore
    totalOutputAdaIncrease =
        Coin.distance totalOutputAdaAfter totalOutputAdaBefore

    totalOutputCostAfter =
        F.foldMap (txOutputCost constraints) outputsAfter
    totalOutputCostBefore =
        F.foldMap (txOutputCost constraints) outputsBefore
    totalOutputCostIncrease =
        Coin.distance totalOutputCostBefore totalOutputCostAfter

--------------------------------------------------------------------------------
-- Minimizing fees for outputs
--------------------------------------------------------------------------------

data MockMinimizeFeeForOutputArguments = MockMinimizeFeeForOutputArguments
    { mockConstraints :: MockTxConstraints
    , mockFeeExcessToMinimize :: Coin
    , mockOutput :: TokenBundle
    }
    deriving (Eq, Show)

genMockMinimizeFeeForOutputArguments :: Gen MockMinimizeFeeForOutputArguments
genMockMinimizeFeeForOutputArguments = do
    mockConstraints <- genMockTxConstraints
    mockOutput <- genTokenBundle
    mockFeeExcessToMinimize <- genCoinRange (Coin 0) (Coin 1000)
    pure MockMinimizeFeeForOutputArguments
        { mockConstraints
        , mockFeeExcessToMinimize
        , mockOutput
        }

instance Arbitrary MockMinimizeFeeForOutputArguments where
    arbitrary = genMockMinimizeFeeForOutputArguments

prop_minimizeFeeForOutput :: Blind MockMinimizeFeeForOutputArguments -> Property
prop_minimizeFeeForOutput mockArgs =
    checkCoverage $
    cover 50 (feeExcessAfter == Coin 0)
        "feeExcessAfter == 0" $
    cover 0.01 (feeExcessAfter /= Coin 0)
        "feeExcessAfter /= 0" $
    cover 1 (outputCostIncrease > Coin 0)
        "outputCostIncrease > 0" $
    counterexample counterexampleText $ conjoinMap
        [ ( "feeExcessAfter > feeExcessBefore"
          , feeExcessAfter <= feeExcessBefore )
        , ( "outputCoinAfter < outputCoinBefore"
          , outputCoinAfter >= outputCoinBefore )
        , ( "outputCostAfter < outputCostBefore"
          , outputCostAfter >= outputCostBefore )
        , ( "feeExcessReduction <> feeExcessAfter /= feeExcessBefore"
          , feeExcessReduction <> feeExcessAfter == feeExcessBefore )
        , ( "costOfEliminatingFeeExcess < gainOfEliminatingFeeExcess"
          , costOfEliminatingFeeExcess >= gainOfEliminatingFeeExcess )
        ]
  where
    Blind MockMinimizeFeeForOutputArguments
        { mockConstraints
        , mockFeeExcessToMinimize
        , mockOutput
        } = mockArgs
    constraints = unMockTxConstraints mockConstraints

    (feeExcessAfter, outputAfter) =
        minimizeFeeForOutput constraints (mockFeeExcessToMinimize, mockOutput)

    costOfEliminatingFeeExcess = Coin.distance
        (txOutputCoinCost constraints outputCoinAfter)
        (txOutputCoinCost constraints (outputCoinAfter <> feeExcessAfter))
    gainOfEliminatingFeeExcess = fromMaybe (Coin 0) $ Coin.subtractCoin
        feeExcessAfter
        costOfEliminatingFeeExcess

    feeExcessBefore =
        mockFeeExcessToMinimize
    feeExcessReduction =
        Coin.distance feeExcessBefore feeExcessAfter

    outputBefore =
        mockOutput
    outputCoinAfter =
        view #coin outputAfter
    outputCoinBefore =
        view #coin mockOutput
    outputCoinIncrease =
        Coin.distance outputCoinBefore outputCoinAfter
    outputCostAfter =
        txOutputCost constraints outputAfter
    outputCostBefore =
        txOutputCost constraints outputBefore
    outputCostIncrease =
        Coin.distance outputCostBefore outputCostAfter

    counterexampleText = counterexampleMap
        [ ( "costOfEliminatingFeeExcess"
          , show costOfEliminatingFeeExcess )
        , ( "gainOfEliminatingFeeExcess"
          , show gainOfEliminatingFeeExcess )
        , ( "feeExcessAfter"
          , show feeExcessAfter )
        , ( "feeExcessBefore"
          , show feeExcessBefore )
        , ( "feeExcessReduction"
          , show feeExcessReduction )
        , ( "outputCoinAfter"
          , show outputCoinAfter )
        , ( "outputCoinBefore"
          , show outputCoinBefore )
        , ( "outputCoinIncrease"
          , show outputCoinIncrease )
        , ( "outputCostAfter"
          , show outputCostAfter )
        , ( "outputCostBefore"
          , show outputCostBefore )
        , ( "outputCostIncrease"
          , show outputCostIncrease )
        ]

--------------------------------------------------------------------------------
-- Cost calculations
--------------------------------------------------------------------------------

-- TODO:
--
-- Find a way to merge the common parts of cost and size calculations.
-- Find a way to test the effect increasing the size of a token quantity.
-- Find a way to test that the largest possible bundle has a cost that does
-- not exceed maxBound :: Word64.

data MockTxOutputCostArguments = MockTxOutputCostArguments
    { mockConstraints :: MockTxConstraints
    , mockOutput :: TokenBundle
    } deriving (Eq, Show)

genMockTxOutputCostArguments :: Gen MockTxOutputCostArguments
genMockTxOutputCostArguments = MockTxOutputCostArguments
    <$> genMockTxConstraints
    <*> genTokenBundle

instance Arbitrary MockTxOutputCostArguments where
    arbitrary = genMockTxOutputCostArguments

prop_txOutputCost :: MockTxOutputCostArguments -> Property
prop_txOutputCost mockArgs = conjoin
    [ txOutputCost constraints mockOutput <
      txOutputCost constraints outputWithLargerCoin
    , txOutputCost constraints mockOutput <
      txOutputCost constraints outputWithMaxCoin
    , Coin.distance
        (txOutputCost constraints mockOutput)
        (txOutputCost constraints outputWithLargerCoin)
      ==
      Coin.distance
        (txOutputCoinCost constraints (view #coin mockOutput))
        (txOutputCoinCost constraints (view #coin outputWithLargerCoin))
    ]
  where
    outputWithLargerCoin =
        TokenBundle.setCoin mockOutput
            $ multiplyCoinByTen
            $ TokenBundle.getCoin mockOutput
    outputWithMaxCoin =
        TokenBundle.setCoin mockOutput maxBound
    MockTxOutputCostArguments
        { mockConstraints
        , mockOutput
        } = mockArgs
    constraints =
        unMockTxConstraints mockConstraints
    multiplyCoinByTen (Coin n) = Coin $ 10 * n

--------------------------------------------------------------------------------
-- Size calculations
--------------------------------------------------------------------------------

data MockTxOutputSizeArguments = MockTxOutputSizeArguments
    { mockConstraints :: MockTxConstraints
    , mockOutput :: TokenBundle
    } deriving (Eq, Show)

genMockTxOutputSizeArguments :: Gen MockTxOutputSizeArguments
genMockTxOutputSizeArguments = MockTxOutputSizeArguments
    <$> genMockTxConstraints
    <*> genTokenBundle

instance Arbitrary MockTxOutputSizeArguments where
    arbitrary = genMockTxOutputSizeArguments

prop_txOutputSize :: MockTxOutputSizeArguments -> Property
prop_txOutputSize mockArgs = conjoin
    [ txOutputSize constraints mockOutput <
      txOutputSize constraints outputWithLargerCoin
    , txOutputSize constraints mockOutput <
      txOutputSize constraints outputWithMaxCoin
    , txSizeDistance
        (txOutputSize constraints mockOutput)
        (txOutputSize constraints outputWithLargerCoin)
      ==
      txSizeDistance
        (txOutputCoinSize constraints (view #coin mockOutput))
        (txOutputCoinSize constraints (view #coin outputWithLargerCoin))
    ]
  where
    outputWithLargerCoin =
        TokenBundle.setCoin mockOutput
            $ multiplyCoinByTen
            $ TokenBundle.getCoin mockOutput
    outputWithMaxCoin =
        TokenBundle.setCoin mockOutput maxBound
    MockTxOutputSizeArguments
        { mockConstraints
        , mockOutput
        } = mockArgs
    constraints =
        unMockTxConstraints mockConstraints
    multiplyCoinByTen (Coin n) = Coin $ 10 * n

--------------------------------------------------------------------------------
-- Mock transaction constraints
--------------------------------------------------------------------------------

data MockTxConstraints = MockTxConstraints
    { mockTxBaseCost
        :: MockTxBaseCost
    , mockTxBaseSize
        :: MockTxBaseSize
    , mockTxCostFactor
        :: MockTxCostFactor
    , mockTxInputSize
        :: MockTxInputSize
    , mockTxOutputMaximumSize
        :: MockTxOutputMaximumSize
    , mockTxOutputMaximumTokenQuantity
        :: MockTxOutputMaximumTokenQuantity
    , mockTxOutputMinimumAdaQuantity
        :: MockTxOutputMinimumAdaQuantity
    , mockTxMaximumSize
        :: MockTxMaximumSize
    }
    deriving (Eq, Generic, Show)

unMockTxConstraints :: MockTxConstraints -> TxConstraints MockSize
unMockTxConstraints MockTxConstraints {..} = TxConstraints
    { txBaseCost =
        unMockTxBaseCost mockTxBaseCost
    , txBaseSize =
        unMockTxBaseSize mockTxBaseSize
    , txInputCost =
        mockSizeToCost $ unMockTxInputSize mockTxInputSize
    , txInputSize =
        unMockTxInputSize mockTxInputSize
    , txOutputCost =
        mockSizeToCost . mockOutputSize
    , txOutputSize =
        mockOutputSize
    , txOutputMaximumSize =
        unMockTxOutputMaximumSize mockTxOutputMaximumSize
    , txOutputMaximumTokenQuantity =
        unMockTxOutputMaximumTokenQuantity mockTxOutputMaximumTokenQuantity
    , txOutputMinimumAdaQuantity =
        unMockTxOutputMinimumAdaQuantity mockTxOutputMinimumAdaQuantity
    , txRewardWithdrawalCost =
        mockSizeToCost . mockRewardWithdrawalSize
    , txRewardWithdrawalSize =
        mockRewardWithdrawalSize
    , txMaximumSize =
        unMockTxMaximumSize mockTxMaximumSize
    }
  where
    mockOutputSize :: TokenBundle -> MockSize
    mockOutputSize (TokenBundle c m) = (<>)
        (MockSize $ fromIntegral $ BS.length $ pretty $ Flat m)
        (mockCoinSize c)

    mockRewardWithdrawalSize :: Coin -> MockSize
    mockRewardWithdrawalSize = \case
        Coin 0 -> MockSize 0
        Coin c -> mockCoinSize (Coin c)

    mockCoinSize :: Coin -> MockSize
    mockCoinSize = MockSize . fromIntegral . length . show

    mockSizeToCost :: MockSize -> Coin
    mockSizeToCost (MockSize s) =
        Coin $ fromIntegral $ fromIntegral m * s
      where
        m = unMockTxCostFactor mockTxCostFactor

genMockTxConstraints :: Gen MockTxConstraints
genMockTxConstraints = MockTxConstraints
    <$> genMockTxBaseCost
    <*> genMockTxBaseSize
    <*> genMockTxCostFactor
    <*> genMockTxInputSize
    <*> genMockTxOutputMaximumSize
    <*> genMockTxOutputMaximumTokenQuantity
    <*> genMockTxOutputMinimumAdaQuantity
    <*> genMockTxMaximumSize

instance Arbitrary MockTxConstraints where
    arbitrary = genMockTxConstraints

--------------------------------------------------------------------------------
-- Mock base transaction costs
--------------------------------------------------------------------------------

newtype MockTxBaseCost = MockTxBaseCost
    { unMockTxBaseCost :: Coin }
    deriving stock Eq
    deriving Show via Coin

genMockTxBaseCost :: Gen MockTxBaseCost
genMockTxBaseCost = MockTxBaseCost <$> genCoinRange (Coin 0) (Coin 1000)

--------------------------------------------------------------------------------
-- Mock base transaction sizes
--------------------------------------------------------------------------------

newtype MockTxBaseSize = MockTxBaseSize
    { unMockTxBaseSize :: MockSize }
    deriving stock Eq
    deriving Show via Natural

genMockTxBaseSize :: Gen MockTxBaseSize
genMockTxBaseSize = MockTxBaseSize <$> genMockSizeRange 0 1000

--------------------------------------------------------------------------------
-- Mock transaction cost factors
--------------------------------------------------------------------------------

newtype MockTxCostFactor = MockTxCostFactor
    { unMockTxCostFactor :: Int }
    deriving stock Eq
    deriving Show via Int

genMockTxCostFactor :: Gen MockTxCostFactor
genMockTxCostFactor = MockTxCostFactor <$> choose (1, 4)

--------------------------------------------------------------------------------
-- Mock input sizes
--------------------------------------------------------------------------------

newtype MockTxInputSize = MockTxInputSize
    { unMockTxInputSize :: MockSize }
    deriving stock Eq
    deriving Show via Natural

genMockTxInputSize :: Gen MockTxInputSize
genMockTxInputSize = MockTxInputSize <$> genMockSizeRange 0 10

--------------------------------------------------------------------------------
-- Mock maximum output sizes
--------------------------------------------------------------------------------

newtype MockTxOutputMaximumSize = MockTxOutputMaximumSize
    { unMockTxOutputMaximumSize :: MockSize }
    deriving stock Eq
    deriving Show via Natural

genMockTxOutputMaximumSize :: Gen MockTxOutputMaximumSize
genMockTxOutputMaximumSize = MockTxOutputMaximumSize
    -- Chosen so that the upper limit is just above the unconstrained maximum
    -- size of token bundles generated by 'genTokenBundle'.
    <$> genMockSizeRange 200 1500

--------------------------------------------------------------------------------
-- Mock maximum token quantities
--------------------------------------------------------------------------------

newtype MockTxOutputMaximumTokenQuantity = MockTxOutputMaximumTokenQuantity
    { unMockTxOutputMaximumTokenQuantity :: TokenQuantity }
    deriving stock Eq
    deriving Show via Natural

genMockTxOutputMaximumTokenQuantity :: Gen MockTxOutputMaximumTokenQuantity
genMockTxOutputMaximumTokenQuantity = MockTxOutputMaximumTokenQuantity <$>
    genTokenQuantityRange (TokenQuantity 100) (TokenQuantity 2000)

--------------------------------------------------------------------------------
-- Mock minimum ada quantities
--------------------------------------------------------------------------------

data MockTxOutputMinimumAdaQuantity = MockTxOutputMinimumAdaQuantity
    { mockMinimumAdaQuantityPerOutput :: Coin
    , mockMinimumAdaQuantityPerOutputAsset :: Coin
    }
    deriving (Eq, Show)

unMockTxOutputMinimumAdaQuantity
    :: MockTxOutputMinimumAdaQuantity
    -> (TokenMap -> Coin)
unMockTxOutputMinimumAdaQuantity mock = \m ->
    let assetCount = Set.size $ TokenMap.getAssets m in
    mockMinimumAdaQuantityPerOutput mock
        <> mtimesDefault assetCount (mockMinimumAdaQuantityPerOutputAsset mock)

genMockTxOutputMinimumAdaQuantity :: Gen MockTxOutputMinimumAdaQuantity
genMockTxOutputMinimumAdaQuantity = MockTxOutputMinimumAdaQuantity
    <$> genCoinRange (Coin 0) (Coin 10)
    <*> genCoinRange (Coin 0) (Coin 10)

--------------------------------------------------------------------------------
-- Mock maximum transaction sizes
--------------------------------------------------------------------------------

newtype MockTxMaximumSize = MockTxMaximumSize
    { unMockTxMaximumSize :: MockSize }
    deriving stock Eq
    deriving Show via Natural

genMockTxMaximumSize :: Gen MockTxMaximumSize
genMockTxMaximumSize =
    MockTxMaximumSize <$> genMockSizeRange 0 10_000

--------------------------------------------------------------------------------
-- Generating inputs
--------------------------------------------------------------------------------

newtype MockInputId = MockInputId
    { unMockInputId :: ByteString }
    deriving (Eq, Ord)

instance Show MockInputId where
    show = T.unpack . T.decodeUtf8 . convertToBase Base16 . unMockInputId

genMockInput :: Gen (MockInputId, TokenBundle)
genMockInput = (,)
    <$> genMockInputId
    <*> genTokenBundle

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
    coin <- genCoinRange (Coin 1) (Coin 1000)
    pure TokenBundle {coin, tokens}
  where
    genAssetQuantity :: Gen (AssetId, TokenQuantity)
    genAssetQuantity = (,)
        <$> genAssetIdLargeRange
        <*> genTokenQuantityRange (TokenQuantity 0) (TokenQuantity 1000)

--------------------------------------------------------------------------------
-- Generating coins
--------------------------------------------------------------------------------

genCoinRange :: Coin -> Coin -> Gen Coin
genCoinRange (Coin minCoin) (Coin maxCoin) =
    Coin . fromIntegral <$> choose (minCoin, maxCoin)

--------------------------------------------------------------------------------
-- Generating token quantities
--------------------------------------------------------------------------------

genTokenQuantityRange :: TokenQuantity -> TokenQuantity -> Gen TokenQuantity
genTokenQuantityRange (TokenQuantity a) (TokenQuantity b) =
    TokenQuantity . fromIntegral @Integer
        <$> choose (fromIntegral a, fromIntegral b)

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

instance TxSize MockSize where
    MockSize a `txSizeDistance` MockSize b
        | a >= b    = MockSize (a - b)
        | otherwise = MockSize (b - a)

genMockSizeRange :: Natural -> Natural -> Gen MockSize
genMockSizeRange minSize maxSize =
    MockSize . fromIntegral @Integer @Natural <$>
        choose (fromIntegral minSize, fromIntegral maxSize)

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
