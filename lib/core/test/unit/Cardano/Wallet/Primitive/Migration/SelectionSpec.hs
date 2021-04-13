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
    , outputSizeWithinLimit
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
    ( TxConstraints (..), txOutputCoinCost, txOutputCoinSize )
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

data MockCreateArguments = MockCreateArguments
    { mockConstraints :: MockTxConstraints
    , mockInputs :: NonEmpty (MockInputId, TokenBundle)
    , mockRewardWithdrawal :: Coin
    } deriving (Eq, Show)

genMockCreateArguments :: Gen MockCreateArguments
genMockCreateArguments = do
    mockConstraints <- genMockTxConstraints
    mockRewardWithdrawal <- genCoinRange (Coin 0) (Coin 100)
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

--------------------------------------------------------------------------------
-- Coalescing token bundles
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
        [ all (outputSizeWithinLimit constraints) result
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
    mockFeeExcessToMinimize <- genCoinRange (Coin 0) (Coin 1000)
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
    cover 5 (totalOutputCostIncrease > Coin 0)
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
    cover 0.01 (outputCostIncrease > Coin 0)
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
    MockTxOutputSizeArguments
        { mockConstraints
        , mockOutput
        } = mockArgs
    constraints =
        unMockTxConstraints mockConstraints
    multiplyCoinByTen (Coin n) = Coin $ 10 * n

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
genCoin = genCoinRange (Coin 1) (Coin 10_000)

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
-- Mock transaction constraints
--------------------------------------------------------------------------------

data MockTxConstraints = MockTxConstraints
    { mockTxCostMultiplier
        :: MockTxCostMultiplier
    , mockTxBaseCost
        :: MockTxBaseCost
    , mockTxBaseSize
        :: MockTxBaseSize
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

unMockTxConstraints
    :: MockTxConstraints -> TxConstraints MockSize
unMockTxConstraints m = TxConstraints
    { txBaseCost =
        unMockTxBaseCost $ mockTxBaseCost m
    , txBaseSize =
        unMockTxBaseSize $ mockTxBaseSize m
    , txInputCost =
        mockSizeToCost (mockTxCostMultiplier m) <$> unMockTxInputSize $ mockTxInputSize m
    , txInputSize =
        unMockTxInputSize $ mockTxInputSize m
    , txOutputCost =
        mockSizeToCost (mockTxCostMultiplier m) . mockOutputSize
    , txOutputSize =
        mockOutputSize
    , txOutputMaximumSize =
        unMockTxOutputMaximumSize $ mockTxOutputMaximumSize m
    , txOutputMaximumTokenQuantity =
        unMockTxOutputMaximumTokenQuantity $ mockTxOutputMaximumTokenQuantity m
    , txOutputMinimumAdaQuantity =
        unMockTxOutputMinimumAdaQuantity $ mockTxOutputMinimumAdaQuantity m
    , txRewardWithdrawalCost =
        mockSizeToCost (mockTxCostMultiplier m) . mockRewardWithdrawalSize
    , txRewardWithdrawalSize =
        mockRewardWithdrawalSize
    , txMaximumSize =
        unMockTxMaximumSize $ mockTxMaximumSize m
    }

genMockTxConstraints :: Gen MockTxConstraints
genMockTxConstraints = MockTxConstraints
    <$> genMockTxCostMultiplier
    <*> genMockTxBaseCost
    <*> genMockTxBaseSize
    <*> genMockTxInputSize
    <*> genMockTxOutputMaximumSize
    <*> genMockTxOutputMaximumTokenQuantity
    <*> genMockTxOutputMinimumAdaQuantity
    <*> genMockTxMaximumSize

instance Arbitrary MockTxConstraints where
    arbitrary = genMockTxConstraints

mockOutputSize :: TokenBundle -> MockSize
mockOutputSize = MockSize . fromIntegral . BS.length . pretty . Flat

mockRewardWithdrawalSize :: Coin -> MockSize
mockRewardWithdrawalSize = \case
    Coin 0 -> MockSize 0
    Coin c -> MockSize $ fromIntegral $ BS.length $ pretty $ Coin c

mockSizeToCost :: MockTxCostMultiplier -> MockSize -> Coin
mockSizeToCost (MockTxCostMultiplier m) (MockSize s) =
    Coin $ fromIntegral (fromIntegral m * s)

--------------------------------------------------------------------------------
-- Mock transaction base costs
--------------------------------------------------------------------------------

newtype MockTxBaseCost = MockTxBaseCost
    { unMockTxBaseCost :: Coin }
    deriving stock Eq
    deriving Show via Coin

genMockTxBaseCost :: Gen MockTxBaseCost
genMockTxBaseCost = MockTxBaseCost
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

instance TxSize MockSize where
    MockSize a `txSizeDistance` MockSize b
        | a >= b    = MockSize (a - b)
        | otherwise = MockSize (b - a)

genMockSizeRange :: Natural -> Natural -> Gen MockSize
genMockSizeRange minSize maxSize =
    MockSize . fromIntegral @Integer @Natural <$>
        choose (fromIntegral minSize, fromIntegral maxSize)

--------------------------------------------------------------------------------
-- Mock transaction cost multipliers
--------------------------------------------------------------------------------

newtype MockTxCostMultiplier = MockTxCostMultiplier
    { unMockTxCostMultiplier :: Int }
    deriving stock (Eq, Generic, Ord)
    deriving Show via Int

genMockTxCostMultiplier :: Gen MockTxCostMultiplier
genMockTxCostMultiplier =
    MockTxCostMultiplier <$> choose (1, 10)

--------------------------------------------------------------------------------
-- Mock base transaction sizes
--------------------------------------------------------------------------------

newtype MockTxBaseSize = MockTxBaseSize
    { unMockTxBaseSize :: MockSize }
    deriving stock (Eq, Generic, Ord)
    deriving Show via Natural

genMockTxBaseSize :: Gen MockTxBaseSize
genMockTxBaseSize =
    MockTxBaseSize <$> genMockSizeRange 0 100

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
-- Mock maximum transaction sizes
--------------------------------------------------------------------------------

newtype MockTxMaximumSize = MockTxMaximumSize
    { unMockTxMaximumSize :: MockSize }
    deriving stock (Eq, Ord)
    deriving Show via Natural

genMockTxMaximumSize :: Gen MockTxMaximumSize
genMockTxMaximumSize =
    MockTxMaximumSize <$> genMockSizeRange 0 10_000

--------------------------------------------------------------------------------
-- Mock maximum token quantities
--------------------------------------------------------------------------------

newtype MockTxOutputMaximumTokenQuantity = MockTxOutputMaximumTokenQuantity
    { unMockTxOutputMaximumTokenQuantity :: TokenQuantity }
    deriving stock (Eq, Ord)
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
