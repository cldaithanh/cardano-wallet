{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.MigrationSpec
    where

-- TODO:
-- Add Quiet Show instances
--
import Prelude

import Cardano.Wallet.Primitive.Migration
    ( Selection (..)
    , SelectionError (..)
    , SelectionInvariantStatus (..)
    , SelectionParameters (..)
    , TokenQuantityAssessment (..)
    , TokenQuantityAssessor (..)
    , checkSelectionInvariant
    , currentSizeOfSelection
    , emptySelection
    , feeForOutputCoin
    , guardSize
    , minimumAdaQuantityForOutputCoin
    , selectionOutputOrdering
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), subtractCoin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..), TokenBundleSizeAssessor (..) )
import Control.Monad
    ( replicateM )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Semigroup
    ( mtimesDefault, stimes )
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
    , conjoin
    , counterexample
    , cover
    , frequency
    , genericShrink
    , oneof
    , property
    , vector
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.MigrationSpec" $

    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Adding a pure ada entry" $ do

        it "prop_addCoin_invariant" $
            property $ True --prop_addCoin_invariant

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

data MockAddCoinData = MockAddCoinData
    { mockSelectionParameters
        :: MockSelectionParameters
    , mockCoins
        :: NonEmpty Coin
    }
    deriving (Eq, Generic, Show)

genMockAddCoinData :: Gen MockAddCoinData
genMockAddCoinData = do
    mockParams <- genMockSelectionParameters
    let params = unMockSelectionParameters mockParams
    coinCount <- choose (1, 10)
    coins <- NE.fromList <$> replicateM coinCount (genMockCoin params)
    pure $ MockAddCoinData mockParams coins
  where
    -- Generates coins that are close to the boundaries of the various
    -- selection parameters.
    genMockCoin :: SelectionParameters s -> Gen Coin
    genMockCoin params = frequency
        [ (1, oneof [genCoinRange (Coin 1     ) (safeCoinPred a), pure a])
        , (1, oneof [genCoinRange (Coin 1 <> a) (safeCoinPred b), pure b])
        , (1, oneof [genCoinRange (Coin 1 <> b) (safeCoinPred c), pure c])
        , (1, oneof [genCoinRange (Coin 1 <> c) (safeCoinPred d), pure d])
        , (8, oneof [genCoinRange (Coin 1 <> d) (safeCoinPred e), pure e])
        ]
      where
        -- Selection parameters, sorted into ascending order:
        [a, b, c] = L.sort $ fmap (params &)
            [ feeForInput
              -- FIXME
            , flip feeForOutputCoin undefined
            , minimumAdaQuantityForOutputCoin
            ]
        -- The sum of all selection parameters, which is guaranteed to be
        -- at least as large as any of the individual selection parameters:
        d = mconcat [a, b, c]
        -- A value that is much higher than any of the selection parameters:
        e = stimes (1000 :: Int) d

instance Arbitrary MockAddCoinData where
    arbitrary = genMockAddCoinData
{-
prop_addCoin_invariant :: MockAddCoinData -> Property
prop_addCoin_invariant (MockAddCoinData mockParams coins) =
    checkCoverage $
    cover 10 (finalInputCount >= 4 && finalInputCount == length coins)
        "every coin was included as an input" $
    cover 10 (finalInputCount >= 4 && finalInputCount < length coins)
        "at least one coin was not included as an input" $
    cover 10 (finalInputCount >= 4 && finalInputCount == finalOutputCount)
        "final input and output counts are identical" $
    cover 10 (finalInputCount >= 4 && finalInputCount > finalOutputCount)
        "final input and output counts are different" $
    conjoin
        [ conjoin (transitionPreservesInvariant <$> transitions)
        , property $ finalInputCount <= length coins
        , property $ finalInputCount >= finalOutputCount
        ]
  where
    finalInputCount :: Int
    finalInputCount = length $ view #inputs $ NE.last selections

    finalOutputCount :: Int
    finalOutputCount = length $ view #outputs $ NE.last selections

    inputs :: NonEmpty (MockInputId, Coin)
    inputs = mockInputIds `NE.zip` coins

    params :: SelectionParameters MockSize
    params = unMockSelectionParameters mockParams

    selections :: NonEmpty MockSelection
    selections =
        NE.scanl (\s i -> fromRight $ addCoin params s i) emptySelection inputs

    transitions :: [(Coin, (MockSelection, MockSelection))]
    transitions = NE.toList coins `zip` consecutivePairs (NE.toList selections)

    transitionPreservesInvariant
        :: (Coin, (MockSelection, MockSelection))
        -> Property
    transitionPreservesInvariant (coin, (initialSelection, finalSelection)) =
        counterexample counterexampleText $ conjoin
          [ checkSelectionInvariant params initialSelection
              === SelectionInvariantHolds
          , checkSelectionInvariant params finalSelection
              === SelectionInvariantHolds
          ]
      where
        counterexampleText = unlines
            [ "Initial selection:"
            , show initialSelection
            , "Final selection:"
            , show finalSelection
            , "Coin added:"
            , show coin
            ]
-}
--------------------------------------------------------------------------------
-- Mock selections
--------------------------------------------------------------------------------

type MockSelection = Selection MockInputId MockSize

genMockSelection :: MockSelectionParameters -> Gen MockSelection
genMockSelection mockParams =
    oneof $ (\g -> g mockParams) <$>
        [ genMockSelectionSmall
        , genMockSelectionHalfFull
        , genMockSelectionNearlyFull
        ]

genMockSelectionSmall :: MockSelectionParameters -> Gen MockSelection
genMockSelectionSmall mockParams = do
    -- lots of possibilities here
    undefined

genMockSelectionHalfFull :: MockSelectionParameters -> Gen MockSelection
genMockSelectionHalfFull mockParams =
    enlargeUntilHalfFull =<< genMockSelectionSmall mockParams
  where
    enlargeUntilHalfFull :: MockSelection -> Gen MockSelection
    enlargeUntilHalfFull s1
        | size s1 >= halfMaximumSizeOfSelection =
            pure s1
        | otherwise = do
            s2 <- genMockSelectionSmall mockParams
            case joinMockSelections mockParams s1 s2 of
                Nothing -> pure s1
                Just s3 -> enlargeUntilHalfFull s3
    params = unMockSelectionParameters mockParams
    halfMaximumSizeOfSelection =
        mockSizeHalfSafe (maximumSizeOfSelection params)

genMockSelectionNearlyFull :: MockSelectionParameters -> Gen MockSelection
genMockSelectionNearlyFull mockParams =
    enlargeUntilNearlyFull =<< genMockSelectionSmall mockParams
  where
    enlargeUntilNearlyFull :: MockSelection -> Gen MockSelection
    enlargeUntilNearlyFull s1 = do
        s2 <- genMockSelectionSmall mockParams
        case joinMockSelections mockParams s1 s2 of
            Nothing -> pure s1
            Just s3 -> enlargeUntilNearlyFull s3

-- Some large ada coins
-- Some small ada coins
-- Some MA bundles with the minimum ada amount
-- Some MA bundles with a larger ada amount

genInputTokenBundle :: MockSelectionParameters -> Gen TokenBundle
genInputTokenBundle params = do
    assetCount <- oneof
        [ pure 0
        , oneof [pure 1, choose (2, 10)]
        ]
    assets <- replicateM assetCount genAssetQuantity
    undefined
  where
    genAssetQuantity :: Gen (AssetId, TokenQuantity)
    genAssetQuantity = undefined

type MockSelectionError = SelectionError MockSize

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
            & L.sortBy (selectionOutputOrdering params)
        , feeExcess
            = feeExcess s1 <> feeExcess s2
        , size
            = size s1 <> size s2
            & flip mockSizeSubtractSafe (sizeOfEmptySelection params)
        }
    params = unMockSelectionParameters mockParams

--------------------------------------------------------------------------------
-- Mock input identifiers
--------------------------------------------------------------------------------

newtype MockInputId = MockInputId
    { unMockInputId :: ByteString
    }
    deriving (Eq, Ord)

instance Show MockInputId where
    show = T.unpack . T.decodeUtf8 . convertToBase Base16 . unMockInputId

genMockInputId :: Gen MockInputId
genMockInputId = MockInputId . BS.pack <$> vector 8

--------------------------------------------------------------------------------
-- Mock selection parameters
--------------------------------------------------------------------------------

data MockSelectionParameters = MockSelectionParameters
    { mockFeeForEmptySelection
        :: MockFeeForEmptySelection
    , mockFeeForInput
        :: MockFeeForInput
    , mockFeeForOutput
        :: MockFeeForOutput
    , mockSizeOfEmptySelection
        :: MockSizeOfEmptySelection
    , mockSizeOfInput
        :: MockSizeOfInput
    , mockSizeOfOutput
        :: MockSizeOfOutput
    , mockMaximumSizeOfSelection
        :: MockMaximumSizeOfSelection
    , mockMinimumAdaQuantityForOutput
        :: MockMinimumAdaQuantityForOutput
    , mockTokenBundleSizeAssessor
        :: MockTokenBundleSizeAssessor
    , mockTokenQuantityAssessor
        :: MockTokenQuantityAssessor
    }
    deriving (Eq, Generic, Show)

unMockSelectionParameters
    :: MockSelectionParameters -> SelectionParameters MockSize
unMockSelectionParameters m = SelectionParameters
    { feeForEmptySelection =
        unMockFeeForEmptySelection
            $ view #mockFeeForEmptySelection m
    , feeForInput =
        unMockFeeForInput
            $ view #mockFeeForInput m
    , feeForOutput =
        unMockFeeForOutput
            $ view #mockFeeForOutput m
    , sizeOfEmptySelection =
        unMockSizeOfEmptySelection
            $ view #mockSizeOfEmptySelection m
    , sizeOfInput =
        unMockSizeOfInput
            $ view #mockSizeOfInput m
    , sizeOfOutput =
        unMockSizeOfOutput
            $ view #mockSizeOfOutput m
    , maximumSizeOfSelection =
        unMockMaximumSizeOfSelection
            $ view #mockMaximumSizeOfSelection m
    , minimumAdaQuantityForOutput =
        unMockMinimumAdaQuantityForOutput
            $ view #mockMinimumAdaQuantityForOutput m
    , tokenBundleSizeAssessor =
        unMockTokenBundleSizeAssessor
            $ view #mockTokenBundleSizeAssessor m
    , tokenQuantityAssessor =
        unMockTokenQuantityAssessor
            $ view #mockTokenQuantityAssessor m
    }

genMockSelectionParameters :: Gen MockSelectionParameters
genMockSelectionParameters = MockSelectionParameters
    <$> genMockFeeForEmptySelection
    <*> genMockFeeForInput
    <*> genMockFeeForOutput
    <*> genMockSizeOfEmptySelection
    <*> genMockSizeOfInput
    <*> genMockSizeOfOutput
    <*> genMockMaximumSizeOfSelection
    <*> genMockMinimumAdaQuantityForOutput
    <*> genMockTokenBundleSizeAssessor
    <*> genMockTokenQuantityAssessor

instance Arbitrary MockSelectionParameters where
    arbitrary = genMockSelectionParameters

--------------------------------------------------------------------------------
-- Mock fees for empty selections
--------------------------------------------------------------------------------

newtype MockFeeForEmptySelection = MockFeeForEmptySelection
    { unMockFeeForEmptySelection :: Coin }
    deriving (Eq, Show)

genMockFeeForEmptySelection :: Gen MockFeeForEmptySelection
genMockFeeForEmptySelection = MockFeeForEmptySelection
    -- TODO: Use a value higher than 0.
    <$> genCoinRange (Coin 0) (Coin 0)

--------------------------------------------------------------------------------
-- Mock fees for inputs
--------------------------------------------------------------------------------

newtype MockFeeForInput = MockFeeForInput
    { unMockFeeForInput :: Coin }
    deriving (Eq, Show)

genMockFeeForInput :: Gen MockFeeForInput
genMockFeeForInput = MockFeeForInput
    <$> genCoinRange (Coin 0) (Coin 10)

instance Arbitrary MockFeeForInput where
    arbitrary = genMockFeeForInput

--------------------------------------------------------------------------------
-- Mock fees for outputs
--------------------------------------------------------------------------------

data MockFeeForOutput = MockFeeForOutput
    { mockMarginalFeePerBundle :: Coin
    , mockMarginalFeePerAsset :: Coin
    }
    deriving (Eq, Show)

unMockFeeForOutput
    :: MockFeeForOutput
    -> (TokenBundle -> Coin)
unMockFeeForOutput m = \b ->
    let assetCount = Set.size $ TokenBundle.getAssets b in
    mockMarginalFeePerBundle m
        <> mtimesDefault assetCount (mockMarginalFeePerAsset m)

genMockFeeForOutput :: Gen MockFeeForOutput
genMockFeeForOutput = MockFeeForOutput
    <$> genCoinRange (Coin 0) (Coin 10)
    <*> genCoinRange (Coin 0) (Coin 10)

--------------------------------------------------------------------------------
-- Mock sizes
--------------------------------------------------------------------------------

data MockSize = MockSize { unMockSize :: Natural }
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
    MockSizeOfEmptySelection <$> genMockSizeRange 0 10

--------------------------------------------------------------------------------
-- Mock sizes of inputs
--------------------------------------------------------------------------------

newtype MockSizeOfInput = MockSizeOfInput
    { unMockSizeOfInput :: MockSize }
    deriving (Eq, Generic, Ord, Show)

genMockSizeOfInput :: Gen MockSizeOfInput
genMockSizeOfInput =
    MockSizeOfInput <$> genMockSizeRange 0 10

--------------------------------------------------------------------------------
-- Mock sizes of outputs
--------------------------------------------------------------------------------

data MockSizeOfOutput = MockSizeOfOutput
    { mockSizePerOutput :: MockSize
    , mockSizePerOutputAsset :: MockSize
    }
    deriving (Eq, Show)

unMockSizeOfOutput
    :: MockSizeOfOutput
    -> (TokenBundle -> MockSize)
unMockSizeOfOutput mock = \b ->
    let assetCount = Set.size $ TokenBundle.getAssets b in
    mockSizePerOutput mock
        <> mtimesDefault assetCount (mockSizePerOutputAsset mock)

genMockSizeOfOutput :: Gen MockSizeOfOutput
genMockSizeOfOutput = MockSizeOfOutput
    <$> genMockSizeRange 0 10
    <*> genMockSizeRange 0 10

--------------------------------------------------------------------------------
-- Mock maximum sizes of selections
--------------------------------------------------------------------------------

newtype MockMaximumSizeOfSelection = MockMaximumSizeOfSelection
    { unMockMaximumSizeOfSelection :: MockSize }
    deriving (Eq, Generic, Ord, Show)

genMockMaximumSizeOfSelection :: Gen MockMaximumSizeOfSelection
genMockMaximumSizeOfSelection =
    MockMaximumSizeOfSelection <$> genMockSizeRange 0 1000

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
-- Mock token bundle size assessors
--------------------------------------------------------------------------------

data MockTokenBundleSizeAssessor
    = NoBundleSizeLimit
      -- ^ Indicates that there is no limit on a token bundle's size.
    | BundleAssetCountUpperLimit Int
      -- ^ Indicates an inclusive upper bound on the number of assets in a
      -- token bundle.
    deriving (Eq, Show)

unMockTokenBundleSizeAssessor
    :: MockTokenBundleSizeAssessor -> TokenBundleSizeAssessor
unMockTokenBundleSizeAssessor m = TokenBundleSizeAssessor $ case m of
    NoBundleSizeLimit ->
        const TokenBundleSizeWithinLimit
    BundleAssetCountUpperLimit upperLimit ->
        \bundle ->
            let assetCount = Set.size $ TokenBundle.getAssets bundle in
            case assetCount `compare` upperLimit of
                LT -> TokenBundleSizeWithinLimit
                EQ -> TokenBundleSizeWithinLimit
                GT -> OutputTokenBundleSizeExceedsLimit

genMockTokenBundleSizeAssessor :: Gen MockTokenBundleSizeAssessor
genMockTokenBundleSizeAssessor = pure NoBundleSizeLimit
    -- TODO

--------------------------------------------------------------------------------
-- Mock token quantity assessors
--------------------------------------------------------------------------------

data MockTokenQuantityAssessor
    = NoTokenQuantityLimit
      -- ^ Indicates that there is no limit on a token quantity.
    | TokenQuantityUpperLimit TokenQuantity
      -- ^ Indicates an inclusive upper bound on a token quantity.
    deriving (Eq, Show)

unMockTokenQuantityAssessor
    :: MockTokenQuantityAssessor -> TokenQuantityAssessor
unMockTokenQuantityAssessor m = TokenQuantityAssessor $ case m of
    NoTokenQuantityLimit ->
        const TokenQuantityWithinLimit
    TokenQuantityUpperLimit upperLimit ->
        \q -> case q `compare` upperLimit of
            LT -> TokenQuantityWithinLimit
            EQ -> TokenQuantityWithinLimit
            GT -> TokenQuantityExceedsLimit

genMockTokenQuantityAssessor :: Gen MockTokenQuantityAssessor
genMockTokenQuantityAssessor = pure NoTokenQuantityLimit
    -- TODO

--------------------------------------------------------------------------------
-- Reusable generators and shrinkers
--------------------------------------------------------------------------------

genCoinRange :: Coin -> Coin -> Gen Coin
genCoinRange (Coin minCoin) (Coin maxCoin) =
    Coin . fromIntegral <$> choose (minCoin, maxCoin)

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
-- Dummy values
--------------------------------------------------------------------------------

nullSelectionParameters :: SelectionParameters MockSize
nullSelectionParameters = SelectionParameters
    { feeForEmptySelection =
        Coin 0
    , feeForInput =
        Coin 0
    , feeForOutput =
        const (Coin 0)
    , sizeOfEmptySelection =
        MockSize 0
    , sizeOfInput =
        MockSize 0
    , sizeOfOutput =
        const (MockSize 0)
    , maximumSizeOfSelection =
        MockSize 0
    , minimumAdaQuantityForOutput =
        const (Coin 0)
    , tokenBundleSizeAssessor =
        TokenBundleSizeAssessor (const TokenBundleSizeWithinLimit)
    , tokenQuantityAssessor =
        TokenQuantityAssessor (const TokenQuantityWithinLimit)
    }

--------------------------------------------------------------------------------
-- Internal types and functions
--------------------------------------------------------------------------------

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

safeCoinPred :: Coin -> Coin
safeCoinPred c = fromMaybe (Coin 0) (c `subtractCoin` Coin 1)
