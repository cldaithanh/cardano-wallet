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

import Cardano.Wallet.Primitive.Migration.Selection
    ( AddEntry
    , Selection (..)
    , SelectionError (..)
    , SelectionFullError (..)
    , SelectionInvariantStatus (..)
    , SelectionOutputSizeAssessment (..)
    , SelectionOutputSizeAssessor (..)
    , SelectionParameters (..)
    , addCoinToFeeExcess
    , checkInvariant
    , initialize
    , outputOrdering
    , outputSizeWithinLimit
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), subtractCoin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
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
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Maybe
    ( fromMaybe )
import Data.Semigroup
    ( mtimesDefault )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Safe
    ( tailMay )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , checkCoverage
    , choose
    , cover
    , genericShrink
    , oneof
    , property
    , suchThat
    , suchThatMap
    , vector
    , (===)
    )

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

        it "prop_addCoinToFeeExcess" $
            property prop_addCoinToFeeExcess

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
    cover 30 (resultIsSelection result)
        "Initialization succeeded" $
    cover 10 (resultHasMoreInputsThanOutputs result)
        "Initialization succeeded with more inputs than outputs" $
    -- TODO: Raise this coverage threshold above 0:
    cover 0 (resultHasMoreThanOneOutput result)
        "Initialization succeeded with more than one output" $
    cover 10 (resultHasNonZeroFeeExcess result)
        "Initialization succeeded with positive fee excess" $
    -- TODO: Raise this coverage threshold above 0:
    cover 0 (resultHasZeroFeeExcess result)
        "Initialization succeeded with zero fee excess" $
    cover 10 (resultHasInsufficientAda result)
        "Initialization failed due to insufficient ada" $
    cover 10 (resultIsFull result)
        "Initialization failed due to the selection being full" $
    case result of
        Left SelectionAdaInsufficient ->
            -- TODO: Check that the ada amount really is insufficient.
            property True
        Left (SelectionFull e) ->
            property (selectionSizeMaximum e < selectionSizeRequired e)
        Right selection ->
            checkInvariant params selection === SelectionInvariantHolds
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

data MockAddEntryArguments v = MockAddEntryArguments
    { mockSelectionParameters :: MockSelectionParameters
    , mockSelection :: MockSelection
    , mockEntry :: (MockInputId, v)
    }
    deriving (Eq, Show)

genMockAddEntryArguments
    :: forall v. (MockSelectionParameters -> Gen v)
    -> Gen (MockAddEntryArguments v)
genMockAddEntryArguments genValue =
    genInner `suchThatMap` eitherToMaybe
  where
    genInner :: Gen (Either MockSelectionError (MockAddEntryArguments v))
    genInner = do
        MockInitializeArguments
            { mockSelectionParameters
            , mockInputs
            , mockRewardWithdrawal
            } <- genMockInitializeArguments
        let params = unMockSelectionParameters mockSelectionParameters
        let result = initialize params mockRewardWithdrawal mockInputs
        case result of
            Left e ->
                pure $ Left e
            Right mockSelection -> do
                mockEntry <- (,)
                    <$> genMockInputId
                    <*> genValue mockSelectionParameters
                pure $ Right MockAddEntryArguments
                    { mockSelectionParameters
                    , mockSelection
                    , mockEntry
                    }

instance Arbitrary (MockAddEntryArguments Coin) where
    arbitrary = genMockAddEntryArguments (const genCoin)

instance Arbitrary (MockAddEntryArguments TokenBundle) where
    arbitrary = genMockAddEntryArguments genTokenBundle

type MockAddEntry v = AddEntry MockSize MockInputId v

prop_addCoinToFeeExcess :: MockAddEntryArguments Coin -> Property
prop_addCoinToFeeExcess mockArgs =
    prop_addEntry mockArgs addCoinToFeeExcess

prop_addEntry :: MockAddEntryArguments v -> MockAddEntry v -> Property
prop_addEntry mockArgs addEntry =
    checkCoverage $
    cover 50 (isRight result)
        "Adding entry succeeded" $
    case result of
        Left _ -> property True
        Right selection ->
            checkInvariant params selection === SelectionInvariantHolds
  where
    MockAddEntryArguments
        { mockSelectionParameters
        , mockSelection
        , mockEntry
        } = mockArgs
    params = unMockSelectionParameters mockSelectionParameters
    result = addEntry params mockSelection mockEntry

--------------------------------------------------------------------------------
-- Mock results
--------------------------------------------------------------------------------

type MockSelection = Selection MockInputId MockSize
type MockSelectionError = SelectionError MockSize
type MockResult = Either MockSelectionError MockSelection

resultIsSelection :: MockResult -> Bool
resultIsSelection = isRight

resultHasMoreInputsThanOutputs :: MockResult -> Bool
resultHasMoreInputsThanOutputs = matchRight $ \selection ->
    F.length (inputs selection) > F.length (outputs selection)

resultHasMoreThanOneOutput :: MockResult -> Bool
resultHasMoreThanOneOutput = matchRight $ \selection ->
    F.length (outputs selection) > 1

resultHasNonZeroFeeExcess :: MockResult -> Bool
resultHasNonZeroFeeExcess = matchRight $ \selection ->
    feeExcess selection > Coin 0

resultHasZeroFeeExcess :: MockResult -> Bool
resultHasZeroFeeExcess = matchRight $ \selection ->
    feeExcess selection == Coin 0

resultHasInsufficientAda :: MockResult -> Bool
resultHasInsufficientAda = matchLeft $ \case
    SelectionAdaInsufficient -> True
    _ -> False

resultIsFull :: MockResult -> Bool
resultIsFull = matchLeft $ \case
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
    genInner `suchThat` outputSizeWithinLimit params
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
genTokenQuantity = TokenQuantity . fromIntegral @Integer <$>
    choose (1, 1000)

--------------------------------------------------------------------------------
-- Generating selections
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Mock selection parameters
--------------------------------------------------------------------------------

-- What we actually need
--
-- feeForEmptySelection
-- maximumSizeOfSelection
-- minimumAdaQuantityForOutput
--
--
--
-- Functions
--
-- sizeOfOutput
-- sizeOfRewardWithdrawal

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
    { feeForEmptySelection =
        unMockFeeForEmptySelection
            $ view #mockFeeForEmptySelection m
    , feeForInput =
        mockSizeToFee <$> unMockSizeOfInput
            $ view #mockSizeOfInput m
    , feeForOutput =
        mockSizeToFee . mockSizeOfOutput
    , feeForRewardWithdrawal =
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
-- Mock fees for inputs
--------------------------------------------------------------------------------

newtype MockFeeForInput = MockFeeForInput
    { unMockFeeForInput :: Coin }
    deriving stock Eq
    deriving Show via Coin

genMockFeeForInput :: Gen MockFeeForInput
genMockFeeForInput = MockFeeForInput
    <$> genCoinRange (Coin 0) (Coin 10)

instance Arbitrary MockFeeForInput where
    arbitrary = genMockFeeForInput

--------------------------------------------------------------------------------
-- Mock fees for outputs
--------------------------------------------------------------------------------

data MockFeeForOutput
    = MockFeeForOutputByShowLength
    deriving (Eq, Show)

unMockFeeForOutput :: MockFeeForOutput -> (TokenBundle -> Coin)
unMockFeeForOutput = \case
    MockFeeForOutputByShowLength ->
        Coin . fromIntegral . length . show

genMockFeeForOutput :: Gen MockFeeForOutput
genMockFeeForOutput = pure MockFeeForOutputByShowLength

--------------------------------------------------------------------------------
-- Mock fees for reward withdrawal
--------------------------------------------------------------------------------

data MockFeeForRewardWithdrawal
    = MockFeeForRewardWithdrawalByShowLength
    deriving (Eq, Show)

unMockFeeForRewardWithdrawal
    :: MockFeeForRewardWithdrawal
    -> (Coin -> Coin)
unMockFeeForRewardWithdrawal = \case
    MockFeeForRewardWithdrawalByShowLength -> \case
        Coin 0 -> Coin 0
        coin -> Coin $ fromIntegral $ length $ show coin

genMockFeeForRewardWithdrawal :: Gen MockFeeForRewardWithdrawal
genMockFeeForRewardWithdrawal = pure MockFeeForRewardWithdrawalByShowLength

instance Arbitrary MockFeeForRewardWithdrawal where
    arbitrary = genMockFeeForRewardWithdrawal

--------------------------------------------------------------------------------
-- Mock sizes
--------------------------------------------------------------------------------

newtype MockSize = MockSize { unMockSize :: Natural }
    deriving (Eq, Generic, Ord, Show)

instance Semigroup MockSize where
    MockSize a <> MockSize b = MockSize (a + b)

instance Monoid MockSize where
    mempty = MockSize 0

genMockSizeRange :: Natural -> Natural -> Gen MockSize
genMockSizeRange minSize maxSize =
    MockSize . fromIntegral @Integer @Natural <$>
        choose (fromIntegral minSize, fromIntegral maxSize)

mockSizeHalfSafe :: MockSize -> MockSize
mockSizeHalfSafe (MockSize a) = MockSize $ a `div` 2

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
unMockMaximumSizeOfOutput _mock = SelectionOutputSizeAssessor assess
  where
    assess = const SelectionOutputSizeWithinLimit

genMockMaximumSizeOfOutput :: Gen MockMaximumSizeOfOutput
genMockMaximumSizeOfOutput = MockMaximumSizeOfOutput
    <$> (Just <$> genMockSizeRange 100 1000)

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
-- Reusable generators and shrinkers
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary Coin where
    arbitrary = oneof
        [ genCoinRange (Coin     1) (Coin     9)
        , genCoinRange (Coin    10) (Coin    99)
        , genCoinRange (Coin   100) (Coin   999)
        , genCoinRange (Coin  1000) (Coin  9999)
        , genCoinRange (Coin 10000) (Coin 99999)
        ]
    shrink = filter (> Coin 0) . genericShrink

--------------------------------------------------------------------------------
-- Internal types and functions
--------------------------------------------------------------------------------

joinMockSelections
    :: MockSelectionParameters
    -> MockSelection
    -> MockSelection
    -> Maybe MockSelection
joinMockSelections mockParams s1 s2
    | size joinedSelection <= maximumSizeOfSelection params =
        Just joinedSelection
    | otherwise =
        Nothing
  where
    joinedSelection = Selection
        { inputs
            = inputs s1 <> inputs s2
        , outputs
            = outputs s2 <> outputs s2
            & NE.sortBy (outputOrdering params)
        , feeExcess
            = feeExcess s1 <> feeExcess s2
        , size
            = size s1 <> size s2
            & flip mockSizeSubtractSafe (sizeOfEmptySelection params)
        , rewardWithdrawal
            = rewardWithdrawal s1 <> rewardWithdrawal s2
        }
    params = unMockSelectionParameters mockParams

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs xs = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "fromJust"

fromRight :: Either e a -> a
fromRight (Right a) = a
fromRight (Left _) = error "fromRight"

matchLeft :: (e -> Bool) -> Either e a -> Bool
matchLeft f result = case result of
    Right _ -> False
    Left x -> f x

matchRight :: (a -> Bool) -> Either e a -> Bool
matchRight f result = case result of
    Right x -> f x
    Left _ -> False

safeCoinPred :: Coin -> Coin
safeCoinPred c = fromMaybe (Coin 0) (c `subtractCoin` Coin 1)
