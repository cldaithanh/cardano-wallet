{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Migration
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), subtractCoin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..), TokenBundleSizeAssessor (..), TxIn )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Monad
    ( foldM )
import Control.Monad.Extra
    ( pureIf )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
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
import qualified Data.Map.Strict as Map

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

emptySelection :: Monoid s => Selection i s
emptySelection = Selection
    { inputs =
        []
    , outputs =
        []
    , feeExcess =
        Coin 0
    , size =
        mempty
    }

initializeSelection
    :: Foldable f
    => Monoid s
    => SelectionParameters s
    -> f (i, TokenBundle)
    -> Maybe (Selection i s)
initializeSelection params entries = do
    selection <- addEntries paramsAdjusted emptySelection entries
    reducedOutputBundles <- reduceOutputAdaQuantities
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

checkSelectionInvariant
    :: (Monoid s, Ord s)
    => SelectionParameters s
    -> Selection i s
    -> SelectionInvariantStatus s
checkSelectionInvariant params selection
    | Just e <- checkSelectionAssetBalance selection =
        SelectionAssetBalanceIncorrect e
    | Just e <- checkSelectionFeeSufficient params selection =
        SelectionFeeInsufficient e
    | Just e <- checkSelectionFeeExcess params selection =
        SelectionFeeExcessIncorrect e
    | Just e <- checkSelectionOutputMinimumAdaQuantities params selection =
        SelectionOutputBelowMinimumAdaQuantity e
    | Just e <- checkSelectionOutputBundleSizes params selection =
        SelectionOutputBundleSizeExceedsLimit e
    | Just e <- checkSelectionOutputOrder params selection =
        SelectionOutputOrderIncorrect e
    | Just e <- checkSelectionOutputTokenQuantities params selection =
        SelectionOutputTokenQuantityExceedsLimit e
    | Just e <- checkSelectionSizeWithinLimit params selection =
        SelectionSizeExceedsLimit e
    | Just e <- checkSelectionSizeCorrectness params selection =
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

checkSelectionAssetBalance
    :: Selection i s
    -> Maybe SelectionAssetBalanceIncorrectError
checkSelectionAssetBalance Selection {inputs, outputs}
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

checkSelectionFeeExcess
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionFeeExcessIncorrectError
checkSelectionFeeExcess params selection =
    check =<< eitherToMaybe (currentFeeForSelection selection)
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
            (minimumFeeForSelection params selection)

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

checkSelectionFeeSufficient
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionFeeInsufficientError
checkSelectionFeeSufficient params selection =
    case currentFeeForSelection selection of
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
    selectionFeeMinimum = minimumFeeForSelection params selection

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

checkSelectionOutputMinimumAdaQuantities
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionOutputBelowMinimumAdaQuantityError
checkSelectionOutputMinimumAdaQuantities params selection =
     maybesToMaybe $ checkSelectionOutput <$> outputs selection
  where
    checkSelectionOutput
        :: TokenBundle -> Maybe SelectionOutputBelowMinimumAdaQuantityError
    checkSelectionOutput outputBundle
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

checkSelectionOutputBundleSizes
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionOutputBundleSizeExceedsLimitError
checkSelectionOutputBundleSizes params selection =
     maybesToMaybe $ checkSelectionOutput <$> outputs selection
  where
    checkSelectionOutput
        :: TokenBundle -> Maybe SelectionOutputBundleSizeExceedsLimitError
    checkSelectionOutput selectionOutputBundle
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

checkSelectionOutputOrder
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionOutputOrderIncorrectError
checkSelectionOutputOrder params selection
    | orderActual == orderExpected =
        Nothing
    | otherwise =
        Just SelectionOutputOrderIncorrectError
  where
    orderActual =
        outputs selection
    orderExpected =
        L.sortBy (selectionOutputOrdering params) (outputs selection)

--------------------------------------------------------------------------------
-- Selection invariant: output token quantities
--------------------------------------------------------------------------------

data SelectionOutputTokenQuantityExceedsLimitError =
    SelectionOutputTokenQuantityExceedsLimitError
    { selectionOutputTokenQuantity :: TokenQuantity
    }
    deriving (Eq, Show)

checkSelectionOutputTokenQuantities
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionOutputTokenQuantityExceedsLimitError
checkSelectionOutputTokenQuantities params selection =
     maybesToMaybe $ checkSelectionOutput <$> outputs selection
  where
    checkSelectionOutput
        :: TokenBundle -> Maybe SelectionOutputTokenQuantityExceedsLimitError
    checkSelectionOutput b
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

checkSelectionSizeCorrectness
    :: (Eq s, Monoid s)
    => SelectionParameters s
    -> Selection i s
    -> Maybe (SelectionSizeIncorrectError s)
checkSelectionSizeCorrectness params selection
    | selectionSizeComputed == selectionSizeStored =
        Nothing
    | otherwise = pure SelectionSizeIncorrectError
        { selectionSizeComputed
        , selectionSizeStored
        }
  where
    selectionSizeComputed = currentSizeOfSelection params selection
    selectionSizeStored = size selection

--------------------------------------------------------------------------------
-- Selection invariant: selection size (in comparison to the limit)
--------------------------------------------------------------------------------

data SelectionSizeExceedsLimitError s = SelectionSizeExceedsLimitError
    { selectionSizeComputed :: s
    , selectionSizeMaximum :: s
    }
    deriving (Eq, Show)

checkSelectionSizeWithinLimit
    :: (Monoid s, Ord s)
    => SelectionParameters s
    -> Selection i s
    -> Maybe (SelectionSizeExceedsLimitError s)
checkSelectionSizeWithinLimit params selection
    | selectionSizeComputed <= selectionSizeMaximum =
        Nothing
    | otherwise = pure SelectionSizeExceedsLimitError
        { selectionSizeComputed
        , selectionSizeMaximum
        }
  where
    selectionSizeComputed = currentSizeOfSelection params selection
    selectionSizeMaximum = maximumSizeOfSelection params

--------------------------------------------------------------------------------
-- Selection parameters
--------------------------------------------------------------------------------

data SelectionParameters s = SelectionParameters
    { feeForEmptySelection :: Coin
      -- ^ The constant fee for an empty selection.
    , feeForInput :: Coin
      -- ^ The constant fee for a selection input.
    , feeForOutput :: TokenBundle -> Coin
      -- ^ The variable fee for a selection output.
    , sizeOfEmptySelection :: s
      -- ^ The constant size of an empty selection.
    , sizeOfInput :: s
      -- ^ The constant size of a selection input.
    , sizeOfOutput :: TokenBundle -> s
      -- ^ The variable size of a selection output.
    , maximumSizeOfSelection :: s
      -- ^ The maximum size of a selection.
    , minimumAdaQuantityForOutput :: TokenMap -> Coin
      -- ^ The variable minimum ada quantity for an output.
    , tokenBundleSizeAssessor :: TokenBundleSizeAssessor
      -- ^ Assesses the size of a token bundle relative to the upper limit of
      -- what can be included in a single selection output.
    , tokenQuantityAssessor :: TokenQuantityAssessor
      -- ^ Assesses a token quantity relative to the upper limit of what can be
      -- included in a selection output.
    }

--------------------------------------------------------------------------------
-- Functions that depend on selection parameters
--------------------------------------------------------------------------------

-- | The amount of ada an output has in excess of its minimum ada quantity.
--
excessAdaForOutput :: SelectionParameters s -> TokenBundle -> Coin
excessAdaForOutput params bundle =
    fromMaybe (Coin 0) result
  where
    result = subtractCoin
        (view #coin bundle)
        (minimumAdaQuantityForOutput params $ view #tokens bundle)

currentSizeOfSelection
    :: Monoid s
    => SelectionParameters s
    -> Selection i s
    -> s
currentSizeOfSelection params selection = mconcat
    [ sizeOfEmptySelection params
    , F.foldMap (const $ sizeOfInput params) (inputs selection)
    , F.foldMap (sizeOfOutput params) (outputs selection)
    ]

-- | The variable fee for an output coin.
--
feeForOutputCoin :: SelectionParameters s -> Coin -> Coin
feeForOutputCoin params = feeForOutput params . TokenBundle.fromCoin

-- | The constant minimum ada quantity for a pure ada output.
--
minimumAdaQuantityForOutputCoin :: SelectionParameters s -> Coin
minimumAdaQuantityForOutputCoin =
    flip minimumAdaQuantityForOutput TokenMap.empty

-- | Calculates the current fee for a selection.
--
currentFeeForSelection :: Selection i s -> Either NegativeCoin Coin
currentFeeForSelection Selection {inputs, outputs}
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
minimumFeeForSelection :: SelectionParameters s -> Selection i s -> Coin
minimumFeeForSelection params selection = mconcat
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
selectionOutputOrdering
    :: SelectionParameters s
    -> TokenBundle
    -> TokenBundle
    -> Ordering
selectionOutputOrdering params =
    comparing (Down . excessAdaForOutput params)

-- | Indicates whether or not the given token bundle is valid for inclusion in
--   a transaction output, according to the given selection parameters.
--
tokenBundleIsValid :: SelectionParameters s -> TokenBundle -> Bool
tokenBundleIsValid params b = and $ (\f -> f params b) <$>
    [ tokenBundleSatisfiesMinimumAdaQuantity
    , tokenBundleSizeWithinLimit
    , tokenBundleQuantitiesWithinLimit
    ]

-- | Indicates whether or not all the quantities within the given bundle are
--   within the limit of what can be included in a transaction output.
--
tokenBundleQuantitiesWithinLimit :: SelectionParameters s -> TokenBundle -> Bool
tokenBundleQuantitiesWithinLimit params b = b
    & view #tokens
    & TokenMap.maximumQuantity
    & tokenQuantityWithinLimit params

-- | Indicates whether or not the ada quantity of the given token bundle is
--   greater than the minimum ada quantity allowed by the protocol.
--
tokenBundleSatisfiesMinimumAdaQuantity
    :: SelectionParameters s
    -> TokenBundle
    -> Bool
tokenBundleSatisfiesMinimumAdaQuantity params (TokenBundle c m) =
    c >= minimumAdaQuantityForOutput params m

-- | Indicates whether or not the size of the given token bundle is within the
--   limit of what can be included in a transaction output.
--
tokenBundleSizeWithinLimit :: SelectionParameters s -> TokenBundle -> Bool
tokenBundleSizeWithinLimit params b = case assess b of
    TokenBundleSizeWithinLimit -> True
    OutputTokenBundleSizeExceedsLimit -> False
  where
    TokenBundleSizeAssessor assess = tokenBundleSizeAssessor params

-- | Indicates whether or not the given token quantity is within the limit of
--   what can be included in a transaction output.
--
tokenQuantityWithinLimit :: SelectionParameters s -> TokenQuantity -> Bool
tokenQuantityWithinLimit params q = case assess q of
    TokenQuantityWithinLimit -> True
    TokenQuantityExceedsLimit -> False
  where
    TokenQuantityAssessor assess = tokenQuantityAssessor params

--------------------------------------------------------------------------------
-- Extending selections
--------------------------------------------------------------------------------

type ExtendSelectionStrategy i s e =
    SelectionParameters s -> Selection i s -> (i, e) -> Maybe (Selection i s)

addEntries
    :: Foldable f
    => SelectionParameters s
    -> Selection i s
    -> f (i, TokenBundle)
    -> Maybe (Selection i s)
addEntries params selection entries =
    foldM (\s e -> addEntry params s e) selection entries

addEntry :: ExtendSelectionStrategy i s TokenBundle
addEntry params selection (inputId, inputBundle)
    | Just inputCoin <- TokenBundle.toCoin inputBundle =
        addCoin params selection (inputId, inputCoin)
    | otherwise =
        addBundle params selection (inputId, inputBundle)

applyFirstSuccessfulStrategy
    :: [ExtendSelectionStrategy i s e]
    -> ExtendSelectionStrategy i s e
applyFirstSuccessfulStrategy strategies params selection input =
    maybesToMaybe $ strategies <&> (\s -> s params selection input)

--------------------------------------------------------------------------------
-- Extending selections with additional coins
--------------------------------------------------------------------------------

addCoin :: ExtendSelectionStrategy i s Coin
addCoin = applyFirstSuccessfulStrategy
    [ addCoinIndependently
    , addCoinToFeeExcess
    ]

addCoinIndependently :: ExtendSelectionStrategy i s Coin
addCoinIndependently params selection (inputId, inputCoin) = do
    outputCoin <- computeOutputCoin
    pureIf (outputCoin >= minimumAdaQuantityForOutputCoin params)
        $ selection
        & addInput (inputId, TokenBundle.fromCoin inputCoin)
        & addOutput params (TokenBundle.fromCoin outputCoin)
  where
    computeOutputCoin :: Maybe Coin
    computeOutputCoin = coinFromInteger
        $ coinToInteger inputCoin
        - coinToInteger (feeForInput params)
        - coinToInteger (feeForOutputCoin params inputCoin)

addCoinToFeeExcess :: ExtendSelectionStrategy i s Coin
addCoinToFeeExcess params selection (inputId, inputCoin) = do
    newFeeExcess <- computeNewFeeExcess
    pureIf (newFeeExcess >= feeExcess selection)
        $ selection {feeExcess = newFeeExcess}
        & addInput (inputId, TokenBundle.fromCoin inputCoin)
  where
    computeNewFeeExcess :: Maybe Coin
    computeNewFeeExcess = coinFromInteger
        $ coinToInteger (feeExcess selection)
        + coinToInteger inputCoin
        - coinToInteger (feeForInput params)

--------------------------------------------------------------------------------
-- Extending selections with additional bundles
--------------------------------------------------------------------------------

addBundle :: ExtendSelectionStrategy i s TokenBundle
addBundle = applyFirstSuccessfulStrategy
    [ addBundleIndependently
    , addBundleByReclaimingAda
    , addBundleToBundle
    , addBundleToCoin
    ]

addBundleIndependently :: ExtendSelectionStrategy i s TokenBundle
addBundleIndependently params selection (inputId, inputBundle) = do
    outputCoin <- computeOutputCoin
    let minimumAdaQuantity = minimumAdaQuantityForOutput params inputMap
    pureIf (outputCoin >= minimumAdaQuantity)
        $ selection
        & addInput (inputId, inputBundle)
        & addOutput params (TokenBundle outputCoin inputMap)
  where
    TokenBundle inputCoin inputMap = inputBundle

    computeOutputCoin :: Maybe Coin
    computeOutputCoin = coinFromInteger
        $ coinToInteger inputCoin
        - coinToInteger (feeForInput params)
        - coinToInteger (feeForOutput params inputBundle)

addBundleByReclaimingAda :: forall i s. ExtendSelectionStrategy i s TokenBundle
addBundleByReclaimingAda params selection (inputId, inputBundle) =
    addInner =<< computeAdaToReclaim
  where
    TokenBundle inputCoin inputMap = inputBundle

    addInner :: Coin -> Maybe (Selection i s)
    addInner adaToReclaim
        | adaToReclaim <= feeExcess selection = do
            let reducedFeeExcess =
                    Coin.distance (feeExcess selection) adaToReclaim
            pure
                $ selection {feeExcess = reducedFeeExcess}
                & addInput (inputId, inputBundle)
                & addOutput params (TokenBundle outputCoin inputMap)
        | otherwise = do
            reducedOutputBundles <- reduceOutputAdaQuantities
                (params)
                (Coin.distance adaToReclaim (feeExcess selection))
                (outputs selection)
            pure
                $ selection
                    { feeExcess = Coin 0
                    , outputs = reducedOutputBundles
                    }
                & addInput (inputId, inputBundle)
                & addOutput params (TokenBundle outputCoin inputMap)
      where
        outputCoin :: Coin
        outputCoin = minimumAdaQuantityForOutput params inputMap

    computeAdaToReclaim :: Maybe Coin
    computeAdaToReclaim = coinFromInteger
        $ coinToInteger (feeForInput params)
        + coinToInteger (feeForOutput params inputBundle)
        + coinToInteger (minimumAdaQuantityForOutput params inputMap)
        - coinToInteger inputCoin

addBundleToBundle :: ExtendSelectionStrategy i s TokenBundle
addBundleToBundle params selection (inputId, inputBundle) = do
    (bundleIndex, mergedBundle) <- findFirstValidMergedBundle
    let (prefix, suffix) = drop 1 <$> L.splitAt bundleIndex (outputs selection)
    pure
        $ selection {outputs = prefix <> suffix}
        & addInput (inputId, inputBundle)
        & addOutput params mergedBundle
  where
    indicesOfBundles :: [(Int, TokenBundle)]
    indicesOfBundles = filter
        (not . TokenBundle.isCoin . snd)
        ([0 ..] `zip` outputs selection)

    findFirstValidMergedBundle :: Maybe (Int, TokenBundle)
    findFirstValidMergedBundle = indicesOfBundles
        & fmap (fmap (<> inputBundle))
        & fmap (fmap (`safeReduceBundleCoin` feeForInput params))
        & filter (tokenBundleIsValid params . snd)
        & listToMaybe
      where
        safeReduceBundleCoin :: TokenBundle -> Coin -> TokenBundle
        safeReduceBundleCoin b reduction
            | TokenBundle.getCoin b <= reduction =
                TokenBundle.setCoin b (Coin 0)
            | otherwise =
                TokenBundle.adjustCoin b (`Coin.distance` reduction)

addBundleToCoin :: ExtendSelectionStrategy i s TokenBundle
addBundleToCoin params selection (inputId, inputBundle) = do
    (prefix, preexistingOutputCoin, suffix) <- findFirstPureAdaOutput
    outputCoin <- computeOutputCoin preexistingOutputCoin
    pureIf (outputCoin >= minimumAdaQuantityForOutput params inputMap)
        $ selection {outputs = prefix <> suffix}
        & addInput (inputId, inputBundle)
        & addOutput params (TokenBundle outputCoin inputMap)
  where
    TokenBundle inputCoin inputMap = inputBundle

    computeOutputCoin :: Coin -> Maybe Coin
    computeOutputCoin preexistingOutputCoin = coinFromInteger
        $ coinToInteger inputCoin
        + coinToInteger preexistingOutputCoin
        - coinToInteger (feeForInput params)

    findFirstPureAdaOutput :: Maybe ([TokenBundle], Coin, [TokenBundle])
    findFirstPureAdaOutput
        | coin : bundles <- suffix =
            Just (prefix, TokenBundle.getCoin coin, bundles)
        | otherwise =
            Nothing
      where
        (prefix, suffix) =
            L.break TokenBundle.isCoin (outputs selection)

--------------------------------------------------------------------------------
-- Classification of coins and token bundles
--------------------------------------------------------------------------------

data ClassifiedUTxO i = ClassifiedUTxO
    { initiators :: [(i, TokenBundle)]
    , supporters :: [(i, TokenBundle)]
    , freeriders :: [(i, TokenBundle)]
    , ignorables :: [(i, TokenBundle)]
    }
    deriving (Eq, Show)

classifyUTxO :: SelectionParameters s -> UTxO -> ClassifiedUTxO TxIn
classifyUTxO params (UTxO u) = ClassifiedUTxO
    { initiators = entriesMatching Initiator
    , supporters = entriesMatching Supporter
    , freeriders = entriesMatching Freerider
    , ignorables = entriesMatching Ignorable
    }
  where
    entries :: [(TxIn, (TokenBundle, TokenBundleClassification))]
    entries =
        fmap ((\b -> (b, classifyTokenBundle params b)) . view #tokens)
            <$> Map.toList u

    entriesMatching :: TokenBundleClassification -> [(TxIn, TokenBundle)]
    entriesMatching classification =
        fmap fst <$> L.filter ((== classification) . snd . snd) entries

data TokenBundleClassification
    = Initiator
    -- ^ A coin or bundle that can be used to single-handedly initialize a
    -- singleton selection. An entry with this classification is capable of
    -- paying for both the base transaction fee and its own marginal fee.
    | Supporter
    -- ^ A coin or bundle that can be used in conjunction with others to
    -- initialize a selection. An entry with this classification is capable of
    -- paying for its own marginal fee, but not capable of paying for the base
    -- transaction fee.
    | Freerider
    -- ^ A bundle that cannot be used to initialize a selection. An entry with
    -- this classification can only be added to a pre-existing selection by
    -- reclaiming ada from other pre-existing outputs, or by merging the value
    -- into a pre-existing output.
    | Ignorable
    -- ^ A coin that should not be added to a selection, because its value is
    -- lower than the marginal fee for an input.
    deriving (Eq, Show)

classifyTokenBundle
    :: SelectionParameters s
    -> TokenBundle
    -> TokenBundleClassification
classifyTokenBundle params b
    | Just c <- TokenBundle.toCoin b, coinIsInitiator c =
        Initiator
    | Just c <- TokenBundle.toCoin b, coinIsIgnorable c =
        Ignorable
    | Just _ <- TokenBundle.toCoin b =
        Supporter
    | bundleIsInitiator b =
        Initiator
    | bundleIsSupporter b =
        Supporter
    | otherwise =
        Freerider
  where
    bundleIsInitiator :: TokenBundle -> Bool
    bundleIsInitiator b@(TokenBundle c m) =
        case computeOutputCoin of
            Nothing -> False
            Just oc -> tokenBundleIsValid params (TokenBundle oc m)
      where
        computeOutputCoin :: Maybe Coin
        computeOutputCoin = coinFromInteger
            $ coinToInteger c
            - coinToInteger (feeForEmptySelection params)
            - coinToInteger (feeForInput params)
            - coinToInteger (feeForOutput params b)

    bundleIsSupporter :: TokenBundle -> Bool
    bundleIsSupporter b@(TokenBundle c m) =
        case computeOutputCoin of
            Nothing -> False
            Just oc -> tokenBundleIsValid params (TokenBundle oc m)
      where
        computeOutputCoin :: Maybe Coin
        computeOutputCoin = coinFromInteger
            $ coinToInteger c
            - coinToInteger (feeForInput params)
            - coinToInteger (feeForOutput params b)

    coinIsInitiator :: Coin -> Bool
    coinIsInitiator c = c >= mconcat
        [ feeForEmptySelection params
        , feeForInput params
        ]

    coinIsIgnorable :: Coin -> Bool
    coinIsIgnorable c = c <= feeForInput params

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
        Just $ insertManyBy (selectionOutputOrdering params)
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
addInput input selection = selection
    { inputs = input : inputs selection }

addOutput
    :: SelectionParameters s
    -> TokenBundle
    -> Selection i s
    -> Selection i s
addOutput params outputBundle selection = selection
    { outputs = L.insertBy
        (selectionOutputOrdering params)
        (outputBundle)
        (outputs selection)
    }

newtype NegativeCoin = NegativeCoin
    { unNegativeCoin :: Coin
    }
    deriving (Eq, Show)

newtype TokenQuantityAssessor = TokenQuantityAssessor
    { assessTokenQuantity :: TokenQuantity -> TokenQuantityAssessment
    }

data TokenQuantityAssessment
    = TokenQuantityWithinLimit
    | TokenQuantityExceedsLimit
    deriving (Eq, Generic, Show)

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

maybesToMaybe :: [Maybe a] -> Maybe a
maybesToMaybe = listToMaybe . catMaybes
