{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Migration.Selection
    where

import Prelude

import Cardano.Wallet.Primitive.Migration.SelectionParameters
    ( SelectionParameters (..)
    , excessAdaForOutput
    , feeForOutputCoin
    , minimumAdaQuantityForOutputCoin
    , sizeOfOutputCoin
    , tokenBundleIsValid
    , tokenBundleSizeWithinLimit
    , tokenBundleQuantitiesWithinLimit
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Control.Monad
    ( foldM )
import Data.Either.Extra
    ( eitherToMaybe, maybeToEither )
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
    ( catMaybes, fromMaybe, listToMaybe )
import Data.Ord
    ( Down (..), comparing )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Selections
--------------------------------------------------------------------------------

data Selection i s = Selection
    { inputs :: ![(i, TokenBundle)]
      -- ^ The selected inputs, in the reverse order to which they were added.
    , outputs :: ![TokenBundle]
      -- ^ The generated outputs, in descending order of excess ada.
    , feeExcess :: !Coin
      -- ^ The excess over the minimum permissible fee for this selection.
    , size :: !s
      -- ^ The current size of this selection.
    }
    deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------
-- Initializing a selection
--------------------------------------------------------------------------------

empty :: Monoid s => Selection i s
empty = Selection
    { inputs =
        []
    , outputs =
        []
    , feeExcess =
        Coin 0
    , size =
        mempty
    }

initialize
    :: Foldable f
    => (Monoid s, Ord s)
    => SelectionParameters s
    -> f (i, TokenBundle)
    -> Either (SelectionError s) (Selection i s)
initialize params entries = do
    selection <- addEntries paramsAdjusted empty entries
    reducedOutputBundles <- maybeToEither SelectionAdaInsufficient $
        reduceOutputAdaQuantities
            params (feeForEmptySelection params) (outputs selection)
    pure selection {outputs = reducedOutputBundles}
  where
    paramsAdjusted = params {feeForEmptySelection = Coin 0}

--------------------------------------------------------------------------------
-- Selection invariants
--------------------------------------------------------------------------------

data SelectionInvariantStatus s
    = SelectionInvariantHolds
    | SelectionAssetBalanceIncorrect
      SelectionAssetBalanceIncorrectError
    | SelectionFeeExcessIncorrect
      SelectionFeeExcessIncorrectError
    | SelectionFeeInsufficient
      SelectionFeeInsufficientError
    | SelectionOutputBelowMinimumAdaQuantity
      SelectionOutputBelowMinimumAdaQuantityError
    | SelectionOutputBundleSizeExceedsLimit
      SelectionOutputBundleSizeExceedsLimitError
    | SelectionOutputOrderIncorrect
      SelectionOutputOrderIncorrectError
    | SelectionOutputTokenQuantityExceedsLimit
      SelectionOutputTokenQuantityExceedsLimitError
    | SelectionSizeExceedsLimit
     (SelectionSizeExceedsLimitError s)
    | SelectionSizeIncorrect
     (SelectionSizeIncorrectError s)
    deriving (Eq, Show)

checkInvariant
    :: (Monoid s, Ord s)
    => SelectionParameters s
    -> Selection i s
    -> SelectionInvariantStatus s
checkInvariant params selection
    | Just e <- checkAssetBalance selection =
        SelectionAssetBalanceIncorrect e
    | Just e <- checkFeeSufficient params selection =
        SelectionFeeInsufficient e
    | Just e <- checkFeeExcess params selection =
        SelectionFeeExcessIncorrect e
    | Just e <- checkOutputMinimumAdaQuantities params selection =
        SelectionOutputBelowMinimumAdaQuantity e
    | Just e <- checkOutputBundleSizes params selection =
        SelectionOutputBundleSizeExceedsLimit e
    | Just e <- checkOutputOrder params selection =
        SelectionOutputOrderIncorrect e
    | Just e <- checkOutputTokenQuantities params selection =
        SelectionOutputTokenQuantityExceedsLimit e
    | Just e <- checkSizeWithinLimit params selection =
        SelectionSizeExceedsLimit e
    | Just e <- checkSizeCorrectness params selection =
        SelectionSizeIncorrect e
    | otherwise =
        SelectionInvariantHolds

--------------------------------------------------------------------------------
-- Selection invariant: asset balance correctness
--------------------------------------------------------------------------------

data SelectionAssetBalanceIncorrectError = SelectionAssetBalanceIncorrectError
    { assetBalanceInputs
        :: TokenMap
    , assetBalanceOutputs
        :: TokenMap
    }
    deriving (Eq, Show)

checkAssetBalance
    :: Selection i s
    -> Maybe SelectionAssetBalanceIncorrectError
checkAssetBalance Selection {inputs, outputs}
    | assetBalanceInputs == assetBalanceOutputs =
        Nothing
    | otherwise =
        Just SelectionAssetBalanceIncorrectError
            { assetBalanceInputs
            , assetBalanceOutputs
            }
  where
    assetBalanceInputs = F.foldMap (tokens . snd) inputs
    assetBalanceOutputs = F.foldMap (tokens) outputs

--------------------------------------------------------------------------------
-- Selection invariant: fee excess correctness
--------------------------------------------------------------------------------

data SelectionFeeExcessIncorrectError = SelectionFeeExcessIncorrectError
    { selectionFeeExcessActual
        :: Coin
    , selectionFeeExcessExpected
        :: Coin
    }
    deriving (Eq, Show)

checkFeeExcess
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionFeeExcessIncorrectError
checkFeeExcess params selection =
    check =<< eitherToMaybe (currentFee selection)
  where
    check :: Coin -> Maybe SelectionFeeExcessIncorrectError
    check currentSelectionFee
        | selectionFeeExcessExpected == selectionFeeExcessActual =
            Nothing
        | otherwise =
            Just SelectionFeeExcessIncorrectError
                { selectionFeeExcessActual
                , selectionFeeExcessExpected
                }
      where
        selectionFeeExcessActual = feeExcess selection
        selectionFeeExcessExpected = Coin.distance
            (currentSelectionFee)
            (minimumFee params selection)

--------------------------------------------------------------------------------
-- Selection invariant: fee sufficiency
--------------------------------------------------------------------------------

data SelectionFeeInsufficientError = SelectionFeeInsufficientError
    { selectionFeeActual
        :: Either NegativeCoin Coin
    , selectionFeeMinimum
        :: Coin
    }
    deriving (Eq, Show)

checkFeeSufficient
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionFeeInsufficientError
checkFeeSufficient params selection =
    case currentFee selection of
        Left nf ->
            Just SelectionFeeInsufficientError
                { selectionFeeActual = Left nf
                , selectionFeeMinimum
                }
        Right pf | pf < selectionFeeMinimum ->
            Just SelectionFeeInsufficientError
                { selectionFeeActual = Right pf
                , selectionFeeMinimum
                }
        Right _ ->
            Nothing
  where
    selectionFeeMinimum = minimumFee params selection

--------------------------------------------------------------------------------
-- Selection invariant: minimum ada quantities
--------------------------------------------------------------------------------

data SelectionOutputBelowMinimumAdaQuantityError =
    SelectionOutputBelowMinimumAdaQuantityError
        { outputBundle :: TokenBundle
          -- ^ The output that is below the expected minimum ada quantity.
        , expectedMinimumAdaQuantity :: Coin
          -- ^ The expected minimum ada quantity.
        }
    deriving (Eq, Show)

checkOutputMinimumAdaQuantities
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionOutputBelowMinimumAdaQuantityError
checkOutputMinimumAdaQuantities params selection =
     maybesToMaybe $ checkOutput <$> outputs selection
  where
    checkOutput
        :: TokenBundle -> Maybe SelectionOutputBelowMinimumAdaQuantityError
    checkOutput outputBundle
        | TokenBundle.getCoin outputBundle >= expectedMinimumAdaQuantity =
            Nothing
        | otherwise =
            Just SelectionOutputBelowMinimumAdaQuantityError
                { outputBundle
                , expectedMinimumAdaQuantity
                }
      where
        expectedMinimumAdaQuantity =
            minimumAdaQuantityForOutput params (view #tokens outputBundle)

--------------------------------------------------------------------------------
-- Selection invariant: output bundle sizes
--------------------------------------------------------------------------------

data SelectionOutputBundleSizeExceedsLimitError =
    SelectionOutputBundleSizeExceedsLimitError
    { selectionOutputBundle :: TokenBundle
    }
    deriving (Eq, Show)

checkOutputBundleSizes
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionOutputBundleSizeExceedsLimitError
checkOutputBundleSizes params selection =
     maybesToMaybe $ checkOutput <$> outputs selection
  where
    checkOutput
        :: TokenBundle -> Maybe SelectionOutputBundleSizeExceedsLimitError
    checkOutput selectionOutputBundle
        | tokenBundleSizeWithinLimit params selectionOutputBundle =
            Nothing
        | otherwise =
            Just SelectionOutputBundleSizeExceedsLimitError
                { selectionOutputBundle }

--------------------------------------------------------------------------------
-- Selection invariant: output ordering
--------------------------------------------------------------------------------

data SelectionOutputOrderIncorrectError =
    SelectionOutputOrderIncorrectError
    deriving (Eq, Show)

checkOutputOrder
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionOutputOrderIncorrectError
checkOutputOrder params selection
    | orderActual == orderExpected =
        Nothing
    | otherwise =
        Just SelectionOutputOrderIncorrectError
  where
    orderActual =
        outputs selection
    orderExpected =
        L.sortBy (outputOrdering params) (outputs selection)

--------------------------------------------------------------------------------
-- Selection invariant: output token quantities
--------------------------------------------------------------------------------

data SelectionOutputTokenQuantityExceedsLimitError =
    SelectionOutputTokenQuantityExceedsLimitError
    { selectionOutputTokenQuantity :: TokenQuantity
    }
    deriving (Eq, Show)

checkOutputTokenQuantities
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionOutputTokenQuantityExceedsLimitError
checkOutputTokenQuantities params selection =
     maybesToMaybe $ checkOutput <$> outputs selection
  where
    checkOutput
        :: TokenBundle -> Maybe SelectionOutputTokenQuantityExceedsLimitError
    checkOutput b
        | tokenBundleQuantitiesWithinLimit params b =
            Nothing
        | otherwise =
            Just SelectionOutputTokenQuantityExceedsLimitError
                { selectionOutputTokenQuantity }
      where
        selectionOutputTokenQuantity =
            TokenMap.maximumQuantity $ view #tokens b

--------------------------------------------------------------------------------
-- Selection invariant: selection size (in comparison to the stored value)
--------------------------------------------------------------------------------

data SelectionSizeIncorrectError s = SelectionSizeIncorrectError
    { selectionSizeComputed :: s
    , selectionSizeStored :: s
    }
    deriving (Eq, Show)

checkSizeCorrectness
    :: (Eq s, Monoid s)
    => SelectionParameters s
    -> Selection i s
    -> Maybe (SelectionSizeIncorrectError s)
checkSizeCorrectness params selection
    | selectionSizeComputed == selectionSizeStored =
        Nothing
    | otherwise = pure SelectionSizeIncorrectError
        { selectionSizeComputed
        , selectionSizeStored
        }
  where
    selectionSizeComputed = currentSize params selection
    selectionSizeStored = size selection

--------------------------------------------------------------------------------
-- Selection invariant: selection size (in comparison to the limit)
--------------------------------------------------------------------------------

data SelectionSizeExceedsLimitError s = SelectionSizeExceedsLimitError
    { selectionSizeComputed :: s
    , selectionSizeMaximum :: s
    }
    deriving (Eq, Show)

checkSizeWithinLimit
    :: (Monoid s, Ord s)
    => SelectionParameters s
    -> Selection i s
    -> Maybe (SelectionSizeExceedsLimitError s)
checkSizeWithinLimit params selection
    | selectionSizeComputed <= selectionSizeMaximum =
        Nothing
    | otherwise = pure SelectionSizeExceedsLimitError
        { selectionSizeComputed
        , selectionSizeMaximum
        }
  where
    selectionSizeComputed = currentSize params selection
    selectionSizeMaximum = maximumSizeOfSelection params

--------------------------------------------------------------------------------
-- Functions that depend on selection parameters
----------------------------------------------------------------------------

currentSize
    :: Monoid s
    => SelectionParameters s
    -> Selection i s
    -> s
currentSize params selection = mconcat
    [ sizeOfEmptySelection params
    , F.foldMap (const $ sizeOfInput params) (inputs selection)
    , F.foldMap (sizeOfOutput params) (outputs selection)
    ]

-- | Calculates the current fee for a selection.
--
currentFee :: Selection i s -> Either NegativeCoin Coin
currentFee Selection {inputs, outputs}
    | adaBalanceIn >= adaBalanceOut =
        Right adaDifference
    | otherwise =
        Left (NegativeCoin adaDifference)
  where
    adaBalanceIn  = F.foldMap (TokenBundle.getCoin . snd) inputs
    adaBalanceOut = F.foldMap (TokenBundle.getCoin) outputs
    adaDifference = Coin.distance adaBalanceIn adaBalanceOut

-- | Calculates the minimum permissible fee for a selection.
--
minimumFee :: SelectionParameters s -> Selection i s -> Coin
minimumFee params selection = mconcat
    [ feeForEmptySelection
    , F.foldMap (const feeForInput) (inputs selection)
    , F.foldMap feeForOutput (outputs selection)
    ]
  where
    SelectionParameters
        { feeForEmptySelection
        , feeForInput
        , feeForOutput
        } = params

-- | Defines the correct ordering of outputs in a selection.
--
outputOrdering
    :: SelectionParameters s
    -> TokenBundle
    -> TokenBundle
    -> Ordering
outputOrdering params =
    comparing (Down . excessAdaForOutput params)

--------------------------------------------------------------------------------
-- Extending selections
--------------------------------------------------------------------------------

type AddEntry s i v =
    SelectionParameters s
        -> Selection i s
        -> (i, v)
        -> Either (SelectionError s) (Selection i s)

data SelectionError s
    = SelectionAdaInsufficient
    | SelectionFull
     (SelectionFullError s)
    deriving (Eq, Show)

data SelectionFullError s = SelectionFullError
    { selectionSizeMaximum :: s
    , selectionSizeRequired :: s
    }
    deriving (Eq, Show)

addEntries
    :: (Foldable f, Monoid s, Ord s)
    => SelectionParameters s
    -> Selection i s
    -> f (i, TokenBundle)
    -> Either (SelectionError s) (Selection i s)
addEntries params =
    foldM $ \selection entry -> addEntry params selection entry

addEntry :: (Monoid s, Ord s) => AddEntry s i TokenBundle
addEntry params selection (inputId, inputBundle)
    | Just inputCoin <- TokenBundle.toCoin inputBundle =
        addCoin params selection (inputId, inputCoin)
    | otherwise =
        addBundle params selection (inputId, inputBundle)
  where
    addCoin :: (Monoid s, Ord s) => AddEntry s i Coin
    addCoin = addEntryWithFirstSuccessfulStrategy
        [ addCoinToFeeExcess
        , addCoinAsNewOutput
        ]

    addBundle :: (Monoid s, Ord s) => AddEntry s i TokenBundle
    addBundle = addEntryWithFirstSuccessfulStrategy
        [ addBundleToExistingOutput
        , addBundleAsNewOutput
        ]

addEntryWithFirstSuccessfulStrategy
    :: NonEmpty (AddEntry s i e)
    -> AddEntry s i e
addEntryWithFirstSuccessfulStrategy strategies params selection input =
    eithersToEither $ strategies <&> (\s -> s params selection input)

--------------------------------------------------------------------------------
-- Adding coins to selections
--------------------------------------------------------------------------------

addCoinToFeeExcess
    :: forall s i. (Monoid s, Ord s)
    => AddEntry s i Coin
addCoinToFeeExcess params selection (inputId, inputCoin) = do
    newFeeExcess <- computeNewFeeExcess
    newSize <- computeNewSize
    pure
        $ selection {feeExcess = newFeeExcess, size = newSize}
        & addInputCoin (inputId, inputCoin)
  where
    computeNewFeeExcess :: Either (SelectionError s) Coin
    computeNewFeeExcess = do
        newFeeExcess <- maybeToEither SelectionAdaInsufficient
              $ coinFromInteger
              $ coinToInteger (feeExcess selection)
              + coinToInteger inputCoin
              - coinToInteger (feeForInput params)
        guardE (newFeeExcess >= feeExcess selection)
            SelectionAdaInsufficient
        pure newFeeExcess

    computeNewSize :: Either (SelectionError s) s
    computeNewSize = guardSize params $ mconcat
        [ size selection
        , sizeOfInput params
        ]

addCoinAsNewOutput
    :: forall s i. (Monoid s, Ord s)
    => AddEntry s i Coin
addCoinAsNewOutput params selection (inputId, inputCoin) = do
    outputCoin <- computeOutputCoin
    newSize <- computeNewSize outputCoin
    pure
        $ selection {size = newSize}
        & addInputCoin (inputId, inputCoin)
        & addOutputCoin params outputCoin
  where
    computeOutputCoin :: Either (SelectionError s) Coin
    computeOutputCoin = do
        outputCoin <- maybeToEither SelectionAdaInsufficient
            $ coinFromInteger
            $ coinToInteger inputCoin
            - coinToInteger (feeForInput params)
            - coinToInteger (feeForOutputCoin params inputCoin)
        guardE (outputCoin >= minimumAdaQuantityForOutputCoin params)
            SelectionAdaInsufficient
        pure outputCoin

    computeNewSize :: Coin -> Either (SelectionError s) s
    computeNewSize outputCoin = guardSize params $ mconcat
        [ size selection
        , sizeOfInput params
        , sizeOfOutputCoin params outputCoin
        ]

--------------------------------------------------------------------------------
-- Adding bundles to selections
--------------------------------------------------------------------------------

addBundleToExistingOutput :: AddEntry s i TokenBundle
addBundleToExistingOutput params selection (inputId, inputBundle) = do
    (bundleIndex, mergedBundle) <- findFirstValidMergedBundle
    let (prefix, suffix) = drop 1 <$> L.splitAt bundleIndex (outputs selection)
    pure
        $ selection {outputs = prefix <> suffix}
        & addInput (inputId, inputBundle)
        & addOutput params mergedBundle
  where
    findFirstValidMergedBundle :: Either (SelectionError s) (Int, TokenBundle)
    findFirstValidMergedBundle = maybeToEither SelectionAdaInsufficient
        $ outputs selection
        & fmap (<> inputBundle)
        & fmap (`safeReduceBundleCoin` feeForInput params)
        & zip [0 ..]
        & filter (tokenBundleIsValid params . snd)
        & listToMaybe
      where
        safeReduceBundleCoin :: TokenBundle -> Coin -> TokenBundle
        safeReduceBundleCoin b reduction
            | TokenBundle.getCoin b <= reduction =
                TokenBundle.setCoin b (Coin 0)
            | otherwise =
                TokenBundle.adjustCoin b (`Coin.distance` reduction)

addBundleAsNewOutput
    :: forall i s. (Monoid s, Ord s)
    => AddEntry s i TokenBundle
addBundleAsNewOutput params selection input@(inputId, inputBundle)
    | adaToReclaim == Coin 0 =
        addBundleAsNewOutputWithoutReclaimingAda
            params selection (inputId, inputBundle)
    | adaToReclaim >= Coin 1 && adaToReclaim <= feeExcess selection = do
        let reducedFeeExcess =
                Coin.distance (feeExcess selection) adaToReclaim
        let inputBundleWithIncreasedAda =
                TokenBundle.adjustCoin inputBundle (<> adaToReclaim)
        updatedSelection <- addBundleAsNewOutputWithoutReclaimingAda
            (params)
            (selection {feeExcess = reducedFeeExcess})
            (inputId, inputBundleWithIncreasedAda)
        pure updatedSelection
            {inputs = replaceHeadOfList (inputs updatedSelection) input}
    | otherwise = do
        reducedOutputBundles <- maybeToEither SelectionAdaInsufficient $
            reduceOutputAdaQuantities
                (params)
                (Coin.distance adaToReclaim (feeExcess selection))
                (outputs selection)
        let inputBundleWithIncreasedAda =
                TokenBundle.adjustCoin inputBundle (<> adaToReclaim)
        updatedSelection <- addBundleAsNewOutputWithoutReclaimingAda
            (params)
            (selection {feeExcess = Coin 0, outputs = reducedOutputBundles})
            (inputId, inputBundleWithIncreasedAda)
        pure updatedSelection
            {inputs = replaceHeadOfList (inputs updatedSelection) input}
  where
    adaToReclaim :: Coin
    adaToReclaim = fromMaybe (Coin 0)
        $ coinFromInteger
        $ coinToInteger (feeForInput params)
        + coinToInteger (feeForOutput params inputBundle)
        + coinToInteger (minimumAdaQuantityForOutput params inputMap)
        - coinToInteger inputCoin

    TokenBundle inputCoin inputMap = inputBundle

addBundleAsNewOutputWithoutReclaimingAda
    :: forall s i. (Monoid s, Ord s)
    => AddEntry s i TokenBundle
addBundleAsNewOutputWithoutReclaimingAda
    params selection (inputId, inputBundle) = do
          outputCoin <- computeOutputCoin
          let outputBundle = TokenBundle outputCoin inputMap
          newSize <- computeNewSize outputBundle
          pure
              $ selection {size = newSize}
              & addInput (inputId, inputBundle)
              & addOutput params outputBundle
  where
    TokenBundle inputCoin inputMap = inputBundle

    computeOutputCoin :: Either (SelectionError s) Coin
    computeOutputCoin = do
        outputCoin <- maybeToEither SelectionAdaInsufficient
            $ coinFromInteger
            $ coinToInteger inputCoin
            - coinToInteger (feeForInput params)
            - coinToInteger (feeForOutput params inputBundle)
        guardE (outputCoin >= minimumAdaQuantityForOutput params inputMap)
            SelectionAdaInsufficient
        pure outputCoin

    computeNewSize :: TokenBundle -> Either (SelectionError s) s
    computeNewSize outputBundle = guardSize params $ mconcat
        [ size selection
        , sizeOfInput params
        , sizeOfOutput params outputBundle
        ]

--------------------------------------------------------------------------------
-- Miscellaneous types and functions
--------------------------------------------------------------------------------

reduceOutputAdaQuantities
    :: SelectionParameters s
    -> Coin
    -> [TokenBundle]
    -> Maybe [TokenBundle]
reduceOutputAdaQuantities params reductionRequired bundles =
    go reductionRequired [] bundles
  where
    go (Coin 0) reducedBundles remainingBundles =
        Just $ insertManyBy (outputOrdering params)
            reducedBundles remainingBundles
    go _ _ [] =
        Nothing
    go coin reducedBundles (bundle : remainingBundles) =
        go reducedCoin (reducedBundle : reducedBundles) remainingBundles
      where
        reduction =
            min coin (excessAdaForOutput params bundle)
        reducedCoin =
            Coin.distance coin reduction
        reducedBundle =
            TokenBundle.adjustCoin bundle (flip Coin.distance reduction)

addInput
    :: (i, TokenBundle)
    -> Selection i s
    -> Selection i s
addInput input selection = selection { inputs = input : inputs selection }

addInputCoin
    :: (i, Coin)
    -> Selection i s
    -> Selection i s
addInputCoin = addInput . fmap TokenBundle.fromCoin

addOutput
    :: SelectionParameters s
    -> TokenBundle
    -> Selection i s
    -> Selection i s
addOutput params outputBundle selection = selection
    { outputs = L.insertBy
        (outputOrdering params)
        (outputBundle)
        (outputs selection)
    }

addOutputCoin
    :: SelectionParameters s
    -> Coin
    -> Selection i s
    -> Selection i s
addOutputCoin params = addOutput params . TokenBundle.fromCoin

newtype NegativeCoin = NegativeCoin
    { unNegativeCoin :: Coin
    }
    deriving (Eq, Show)

guardE :: Bool -> e -> Either e ()
guardE = undefined

guardSize :: Ord s => SelectionParameters s -> s -> Either (SelectionError s) s
guardSize params selectionSizeRequired
    | selectionSizeRequired <= selectionSizeMaximum =
        pure selectionSizeRequired
    | otherwise =
        Left $ SelectionFull SelectionFullError
            { selectionSizeMaximum
            , selectionSizeRequired
            }
  where
    selectionSizeMaximum = maximumSizeOfSelection params

replaceHeadOfList :: [a] -> a -> [a]
replaceHeadOfList as a = case as of
    _ : xs -> a : xs
    [] -> []

coinFromInteger :: Integer -> Maybe Coin
coinFromInteger i
    | i < fromIntegral (unCoin $ minBound @Coin) = Nothing
    | i > fromIntegral (unCoin $ maxBound @Coin) = Nothing
    | otherwise = Just $ Coin $ fromIntegral i

coinToInteger :: Coin -> Integer
coinToInteger = fromIntegral . unCoin

insertManyBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
insertManyBy order itemsToInsert sortedItems =
    L.foldl' f sortedItems itemsToInsert
  where
    f acc itemToInsert = L.insertBy order itemToInsert acc

eithersToEither :: NonEmpty (Either e a) -> Either e a
eithersToEither eithers
    | Just success <- maybesToMaybe $ NE.toList (eitherToMaybe <$> eithers) =
        pure success
    | otherwise =
        NE.head eithers

maybesToMaybe :: [Maybe a] -> Maybe a
maybesToMaybe = listToMaybe . catMaybes
