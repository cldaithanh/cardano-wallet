{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Migration.Selection
    (
    ----------------------------------------------------------------------------
    -- Public interface
    ----------------------------------------------------------------------------

    -- * Classes
      Size (..)

    -- * Selection parameters
    , SelectionParameters (..)
    , SelectionOutputSizeAssessor (..)
    , SelectionOutputSizeAssessment (..)

    -- * Selections
    , Selection (..)
    , SelectionError (..)
    , SelectionFullError (..)

    -- * Initializing a selection
    , initialize

    -- * Finalizing a selection
    , finalize

    -- * Extending a selection
    , AddEntry
    , addEntry
    , addRewardWithdrawal

    ----------------------------------------------------------------------------
    -- Internal interface
    ----------------------------------------------------------------------------

    -- * Selection parameter functions
    , excessAdaForOutput
    , feeForOutputCoin
    , minimumAdaQuantityForOutputCoin
    , sizeOfOutputCoin
    , outputIsValid
    , outputSatisfiesMinimumAdaQuantity
    , outputSizeWithinLimit

    -- * Selection invariants
    , SelectionInvariantStatus (..)
    , checkInvariant

    -- * Selection queries
    , currentFee
    , currentSize
    , minimumFee
    , outputOrdering

    -- * Adding entries to selections
    , addCoinToFeeExcess
    , addBundleToExistingOutput
    , addBundleAsNewOutput
    , addBundleAsNewOutputWithoutReclaimingAda

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), subtractCoin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
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
    ( catMaybes, fromMaybe, mapMaybe, listToMaybe )
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
-- Selection parameters
--------------------------------------------------------------------------------

data SelectionParameters s = SelectionParameters
    { feeForEmptySelection :: Coin
      -- ^ The constant fee for an empty selection.
    , feeForInput :: Coin
      -- ^ The constant fee for a selection input.
    , feeForOutput :: TokenBundle -> Coin
      -- ^ The variable fee for a selection output.
    , feeForRewardWithdrawal :: Coin -> Coin
      -- ^ The variable fee for a reward withdrawal.
    , sizeOfEmptySelection :: s
      -- ^ The constant size of an empty selection.
    , sizeOfInput :: s
      -- ^ The constant size of a selection input.
    , sizeOfOutput :: TokenBundle -> s
      -- ^ The variable size of a selection output.
    , sizeOfRewardWithdrawal :: Coin -> s
      -- ^ The variable size of a reward withdrawal.
    , maximumSizeOfOutput :: SelectionOutputSizeAssessor
      -- ^ The maximum size of a selection output.
    , maximumSizeOfSelection :: s
      -- ^ The maximum size of a selection.
    , minimumAdaQuantityForOutput :: TokenMap -> Coin
      -- ^ The variable minimum ada quantity for an output.
    }

newtype SelectionOutputSizeAssessor = SelectionOutputSizeAssessor
    { assessOutputSize :: TokenBundle -> SelectionOutputSizeAssessment }

data SelectionOutputSizeAssessment
    = SelectionOutputSizeWithinLimit
    | SelectionOutputSizeExceedsLimit
    deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------
-- Selection parameter functions
--------------------------------------------------------------------------------

excessAdaForOutput :: SelectionParameters s -> TokenBundle -> Coin
excessAdaForOutput params bundle =
    fromMaybe (Coin 0) result
  where
    result = subtractCoin
        (view #coin bundle)
        (minimumAdaQuantityForOutput params $ view #tokens bundle)

feeForOutputCoin :: SelectionParameters s -> Coin -> Coin
feeForOutputCoin params = feeForOutput params . TokenBundle.fromCoin

minimumAdaQuantityForOutputCoin :: SelectionParameters s -> Coin
minimumAdaQuantityForOutputCoin =
    flip minimumAdaQuantityForOutput TokenMap.empty

sizeOfOutputCoin :: SelectionParameters s -> Coin -> s
sizeOfOutputCoin = undefined

outputIsValid :: forall s. Ord s => SelectionParameters s -> TokenBundle -> Bool
outputIsValid params b = and $ conditions <&> (\f -> f params b)
  where
    conditions :: [SelectionParameters s -> TokenBundle -> Bool]
    conditions =
        [ outputSatisfiesMinimumAdaQuantity
        , outputSizeWithinLimit
        ]

outputSatisfiesMinimumAdaQuantity
    :: SelectionParameters s -> TokenBundle -> Bool
outputSatisfiesMinimumAdaQuantity params (TokenBundle c m) =
    c >= minimumAdaQuantityForOutput params m

outputSizeWithinLimit :: SelectionParameters s -> TokenBundle -> Bool
outputSizeWithinLimit params b = case assess b of
    SelectionOutputSizeWithinLimit -> True
    SelectionOutputSizeExceedsLimit -> False
  where
    SelectionOutputSizeAssessor assess = maximumSizeOfOutput params

--------------------------------------------------------------------------------
-- Selections
--------------------------------------------------------------------------------

data Selection i s = Selection
    { inputs :: NonEmpty (i, TokenBundle)
      -- ^ The selected inputs, in the reverse order to which they were added.
    , outputs :: NonEmpty TokenBundle
      -- ^ The generated outputs, in descending order of excess ada.
    , feeExcess :: !Coin
      -- ^ The excess over the minimum permissible fee for this selection.
    , size :: !s
      -- ^ The current size of this selection.
    , rewardWithdrawal :: !Coin
      -- ^ The reward withdrawal amount, if any.
    }
    deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------
-- Selection invariants
--------------------------------------------------------------------------------

data SelectionInvariantStatus s
    = SelectionInvariantHolds
    | SelectionInvariantAssetBalanceIncorrect
      SelectionInvariantAssetBalanceIncorrectError
    | SelectionInvariantFeeExcessIncorrect
      SelectionInvariantFeeExcessIncorrectError
    | SelectionInvariantFeeInsufficient
      SelectionInvariantFeeInsufficientError
    | SelectionInvariantOutputBelowMinimumAdaQuantity
      SelectionInvariantOutputBelowMinimumAdaQuantityError
    | SelectionInvariantOutputSizeExceedsLimit
      SelectionInvariantOutputSizeExceedsLimitError
    | SelectionInvariantOutputOrderIncorrect
      SelectionInvariantOutputOrderIncorrectError
    | SelectionInvariantSizeExceedsLimit
     (SelectionInvariantSizeExceedsLimitError s)
    | SelectionInvariantSizeIncorrect
     (SelectionInvariantSizeIncorrectError s)
    deriving (Eq, Show)

checkInvariant
    :: (Monoid s, Ord s)
    => SelectionParameters s
    -> Selection i s
    -> SelectionInvariantStatus s
checkInvariant params selection
    | Just e <- checkAssetBalance selection =
        SelectionInvariantAssetBalanceIncorrect e
    | Just e <- checkFeeSufficient params selection =
        SelectionInvariantFeeInsufficient e
    | Just e <- checkFeeExcess params selection =
        SelectionInvariantFeeExcessIncorrect e
    | Just e <- checkOutputMinimumAdaQuantities params selection =
        SelectionInvariantOutputBelowMinimumAdaQuantity e
    | Just e <- checkOutputSizes params selection =
        SelectionInvariantOutputSizeExceedsLimit e
    | Just e <- checkOutputOrder params selection =
        SelectionInvariantOutputOrderIncorrect e
    | Just e <- checkSizeWithinLimit params selection =
        SelectionInvariantSizeExceedsLimit e
    | Just e <- checkSizeCorrectness params selection =
        SelectionInvariantSizeIncorrect e
    | otherwise =
        SelectionInvariantHolds

--------------------------------------------------------------------------------
-- Selection invariant: asset balance correctness
--------------------------------------------------------------------------------

data SelectionInvariantAssetBalanceIncorrectError =
    SelectionInvariantAssetBalanceIncorrectError
        { assetBalanceInputs
            :: TokenMap
        , assetBalanceOutputs
            :: TokenMap
        }
    deriving (Eq, Show)

checkAssetBalance
    :: Selection i s
    -> Maybe SelectionInvariantAssetBalanceIncorrectError
checkAssetBalance Selection {inputs, outputs}
    | assetBalanceInputs == assetBalanceOutputs =
        Nothing
    | otherwise =
        Just SelectionInvariantAssetBalanceIncorrectError
            { assetBalanceInputs
            , assetBalanceOutputs
            }
  where
    assetBalanceInputs = F.foldMap (tokens . snd) inputs
    assetBalanceOutputs = F.foldMap (tokens) outputs

--------------------------------------------------------------------------------
-- Selection invariant: fee excess correctness
--------------------------------------------------------------------------------

data SelectionInvariantFeeExcessIncorrectError =
    SelectionInvariantFeeExcessIncorrectError
        { selectionFeeExcessActual
            :: Coin
        , selectionFeeExcessExpected
            :: Coin
        }
    deriving (Eq, Show)

checkFeeExcess
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionInvariantFeeExcessIncorrectError
checkFeeExcess params selection =
    check =<< eitherToMaybe (currentFee selection)
  where
    check :: Coin -> Maybe SelectionInvariantFeeExcessIncorrectError
    check currentSelectionFee
        | selectionFeeExcessExpected == selectionFeeExcessActual =
            Nothing
        | otherwise =
            Just SelectionInvariantFeeExcessIncorrectError
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

data SelectionInvariantFeeInsufficientError =
    SelectionInvariantFeeInsufficientError
        { selectionFeeActual
            :: Either NegativeCoin Coin
        , selectionFeeMinimum
            :: Coin
        }
    deriving (Eq, Show)

checkFeeSufficient
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionInvariantFeeInsufficientError
checkFeeSufficient params selection =
    case currentFee selection of
        Left nf ->
            Just SelectionInvariantFeeInsufficientError
                { selectionFeeActual = Left nf
                , selectionFeeMinimum
                }
        Right pf | pf < selectionFeeMinimum ->
            Just SelectionInvariantFeeInsufficientError
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

data SelectionInvariantOutputBelowMinimumAdaQuantityError =
    SelectionInvariantOutputBelowMinimumAdaQuantityError
        { outputBundle :: TokenBundle
          -- ^ The output that is below the expected minimum ada quantity.
        , expectedMinimumAdaQuantity :: Coin
          -- ^ The expected minimum ada quantity.
        }
    deriving (Eq, Show)

checkOutputMinimumAdaQuantities
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionInvariantOutputBelowMinimumAdaQuantityError
checkOutputMinimumAdaQuantities params selection =
     maybesToMaybe $ checkOutput <$> outputs selection
  where
    checkOutput
        :: TokenBundle
        -> Maybe SelectionInvariantOutputBelowMinimumAdaQuantityError
    checkOutput outputBundle
        | TokenBundle.getCoin outputBundle >= expectedMinimumAdaQuantity =
            Nothing
        | otherwise =
            Just SelectionInvariantOutputBelowMinimumAdaQuantityError
                { outputBundle
                , expectedMinimumAdaQuantity
                }
      where
        expectedMinimumAdaQuantity =
            minimumAdaQuantityForOutput params (view #tokens outputBundle)

--------------------------------------------------------------------------------
-- Selection invariant: output sizes
--------------------------------------------------------------------------------

data SelectionInvariantOutputSizeExceedsLimitError =
    SelectionInvariantOutputSizeExceedsLimitError
        { selectionOutput :: TokenBundle }
    deriving (Eq, Show)

checkOutputSizes
    :: Ord s
    => SelectionParameters s
    -> Selection i s
    -> Maybe SelectionInvariantOutputSizeExceedsLimitError
checkOutputSizes params selection =
     maybesToMaybe $ checkOutput <$> outputs selection
  where
    checkOutput
        :: TokenBundle
        -> Maybe SelectionInvariantOutputSizeExceedsLimitError
    checkOutput selectionOutput
        | outputSizeWithinLimit params selectionOutput =
            Nothing
        | otherwise =
            Just SelectionInvariantOutputSizeExceedsLimitError
                { selectionOutput }

--------------------------------------------------------------------------------
-- Selection invariant: output ordering
--------------------------------------------------------------------------------

data SelectionInvariantOutputOrderIncorrectError =
    SelectionInvariantOutputOrderIncorrectError
    deriving (Eq, Show)

checkOutputOrder
    :: SelectionParameters s
    -> Selection i s
    -> Maybe SelectionInvariantOutputOrderIncorrectError
checkOutputOrder params selection
    | orderActual == orderExpected =
        Nothing
    | otherwise =
        Just SelectionInvariantOutputOrderIncorrectError
  where
    orderActual =
        outputs selection
    orderExpected =
        NE.sortBy (outputOrdering params) (outputs selection)

--------------------------------------------------------------------------------
-- Selection invariant: selection size (in comparison to the stored value)
--------------------------------------------------------------------------------

data SelectionInvariantSizeIncorrectError s =
    SelectionInvariantSizeIncorrectError
        { selectionSizeComputed :: s
        , selectionSizeStored :: s
        }
    deriving (Eq, Show)

checkSizeCorrectness
    :: (Eq s, Monoid s)
    => SelectionParameters s
    -> Selection i s
    -> Maybe (SelectionInvariantSizeIncorrectError s)
checkSizeCorrectness params selection
    | selectionSizeComputed == selectionSizeStored =
        Nothing
    | otherwise = pure SelectionInvariantSizeIncorrectError
        { selectionSizeComputed
        , selectionSizeStored
        }
  where
    selectionSizeComputed = currentSize params selection
    selectionSizeStored = size selection

--------------------------------------------------------------------------------
-- Selection invariant: selection size (in comparison to the limit)
--------------------------------------------------------------------------------

data SelectionInvariantSizeExceedsLimitError s =
    SelectionInvariantSizeExceedsLimitError
        { selectionSizeComputed :: s
        , selectionSizeMaximum :: s
        }
    deriving (Eq, Show)

checkSizeWithinLimit
    :: (Monoid s, Ord s)
    => SelectionParameters s
    -> Selection i s
    -> Maybe (SelectionInvariantSizeExceedsLimitError s)
checkSizeWithinLimit params selection
    | selectionSizeComputed <= selectionSizeMaximum =
        Nothing
    | otherwise = pure SelectionInvariantSizeExceedsLimitError
        { selectionSizeComputed
        , selectionSizeMaximum
        }
  where
    selectionSizeComputed = currentSize params selection
    selectionSizeMaximum = maximumSizeOfSelection params

--------------------------------------------------------------------------------
-- Selection query functions
--------------------------------------------------------------------------------

-- | Calculates the current fee for a selection.
--
currentFee :: Selection i s -> Either NegativeCoin Coin
currentFee Selection {inputs, outputs, rewardWithdrawal}
    | adaBalanceIn >= adaBalanceOut =
        Right adaDifference
    | otherwise =
        Left (NegativeCoin adaDifference)
  where
    adaBalanceIn =
        F.foldMap (TokenBundle.getCoin . snd) inputs <> rewardWithdrawal
    adaBalanceOut =
        F.foldMap (TokenBundle.getCoin) outputs
    adaDifference =
        Coin.distance adaBalanceIn adaBalanceOut

-- | Calculates the current size of a selection.
--
currentSize
    :: Monoid s
    => SelectionParameters s
    -> Selection i s
    -> s
currentSize params selection = mconcat
    [ sizeOfEmptySelection params
    , F.foldMap (const $ sizeOfInput params) (inputs selection)
    , F.foldMap (sizeOfOutput params) (outputs selection)
    , sizeOfRewardWithdrawal params (rewardWithdrawal selection)
    ]

-- | Calculates the minimum permissible fee for a selection.
--
minimumFee :: SelectionParameters s -> Selection i s -> Coin
minimumFee params selection = mconcat
    [ feeForEmptySelection params
    , F.foldMap (const $ feeForInput params) (inputs selection)
    , F.foldMap (feeForOutput params) (outputs selection)
    , feeForRewardWithdrawal params (rewardWithdrawal selection)
    ]

-- | Defines the correct ordering of outputs in a selection.
--
outputOrdering
    :: SelectionParameters s
    -> TokenBundle
    -> TokenBundle
    -> Ordering
outputOrdering params = comparing (Down . excessAdaForOutput params)

--------------------------------------------------------------------------------
-- Selection errors
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Initializing a selection
--------------------------------------------------------------------------------

initialize
    :: forall i s. Size s
    => SelectionParameters s
    -> Coin
    -> NonEmpty (i, TokenBundle)
    -> Either (SelectionError s) (Selection i s)
initialize params rewardWithdrawal inputs = do
    let coalescedBundles = NE.sortBy
            (outputOrdering params)
            (coalesceTokenBundles params $ snd <$> inputs)
    let selection = Selection
            { inputs
            , outputs = coalescedBundles
            , feeExcess = Coin 0
            , size = mempty
            , rewardWithdrawal
            }
    outputs <- maybeToEither SelectionAdaInsufficient $
        reduceOutputAdaQuantities
            (params)
            (minimumFee params selection)
            (coalescedBundles)
    size <- guardSize params $ currentSize params selection {outputs}
    -- We have already reduced the output ada quantities in order to pay for
    -- the fee. But this might in turn have reduced the required minimum fee.
    -- So we need to recalculate the fee excess here, as it might be greater
    -- than zero:
    feeExcess <- do
        let mf = minimumFee params selection {outputs, size}
        case currentFee selection {outputs, size} of
            Right cf | cf >= mf ->
                pure (Coin.distance cf mf)
            _ ->
                Left SelectionAdaInsufficient
    pure selection {outputs, size, feeExcess}

--------------------------------------------------------------------------------
-- Finalizing a selection
--------------------------------------------------------------------------------

finalize :: Selection i s -> Selection i s
finalize selection = selection
    -- TODO: Increasing ada quantities of outputs might increase the minimum
    -- required fee, and it might increase the size of the selection beyond the
    -- maximum size, which would render this calculation invalid. We should
    -- probably use an algorithm that increases the quantities incrementally,
    -- until the fee excess can be reduced no further.
    { feeExcess = Coin 0
    , outputs = increaseOutputAdaQuantities
        (feeExcess selection)
        (outputs selection)
    }

--------------------------------------------------------------------------------
-- Extending a selection
--------------------------------------------------------------------------------

type AddEntry s i v =
    SelectionParameters s
        -> Selection i s
        -> (i, v)
        -> Either (SelectionError s) (Selection i s)

addEntry
    :: forall s i. Size s
    => AddEntry s i TokenBundle
addEntry params selection (inputId, inputBundle)
    | Just inputCoin <- TokenBundle.toCoin inputBundle =
        addCoin params selection (inputId, inputCoin)
    | otherwise =
        addBundle params selection (inputId, inputBundle)
  where
    addCoin :: AddEntry s i Coin
    addCoin = addEntryWithFirstSuccessfulStrategy
        [ addCoinAsNewOutput
        -- Consider adding this to an existing output instead. This is because
        -- raising the fee excess is risky. If we accumulate a fee excess that
        -- is too large, when we come to finalize the selection, we might not
        -- be able to redistribute the fee excess back to the outputs without
        -- causing the selection to exceed the maximum size.
        , addCoinToFeeExcess
        ]

    addBundle :: AddEntry s i TokenBundle
    addBundle = addEntryWithFirstSuccessfulStrategy
        [ addBundleToExistingOutput
        , addBundleAsNewOutput
        ]

addEntryWithFirstSuccessfulStrategy
    :: NonEmpty (AddEntry s i e)
    -> AddEntry s i e
addEntryWithFirstSuccessfulStrategy strategies params selection input =
    eithersToEither $ strategies <&> (\s -> s params selection input)

addRewardWithdrawal
    :: Selection i s
    -> Coin
    -> Selection i s
addRewardWithdrawal selection withdrawal = selection
    -- TODO: check that the invariant is not violated.
    { rewardWithdrawal = rewardWithdrawal selection <> withdrawal
    , feeExcess = feeExcess selection <> withdrawal
    }

--------------------------------------------------------------------------------
-- Adding coins to a selection
--------------------------------------------------------------------------------

addCoinToFeeExcess :: forall s i. Size s => AddEntry s i Coin
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

addCoinAsNewOutput :: forall s i. Size s => AddEntry s i Coin
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
-- Adding bundles to a selection
--------------------------------------------------------------------------------

class (Ord a, Monoid a) => Size a where
    sizeDistance :: a -> a -> a

addBundleToExistingOutput :: forall s i. Size s => AddEntry s i TokenBundle
addBundleToExistingOutput params selection (inputId, inputBundle) = do
    (bundleIndex, originalBundle, mergedBundle) <- findFirstValidMergedBundle
    newFeeExcess <- computeNewFeeExcess originalBundle mergedBundle
    newSize <- computeNewSize originalBundle mergedBundle
    let (prefix, suffix) = drop 1 <$> NE.splitAt bundleIndex (outputs selection)
    let remainingOutputs = prefix <> suffix
    case remainingOutputs of
        [] -> pure
            $ selection
                { size = newSize
                , feeExcess = newFeeExcess
                , outputs = mergedBundle :| []
                }
            & addInput (inputId, inputBundle)
        o : os -> pure
            $ selection
                { size = newSize
                , feeExcess = newFeeExcess
                , outputs = o :| os
                }
            & addInput (inputId, inputBundle)
            & addOutput params mergedBundle
  where
    computeNewFeeExcess
        :: TokenBundle -> TokenBundle -> Either (SelectionError s) Coin
    computeNewFeeExcess originalBundle mergedBundle =
        maybeToEither SelectionAdaInsufficient $ coinFromInteger
            $ coinToInteger (feeExcess selection)
            + coinToInteger (TokenBundle.getCoin inputBundle)
            + coinToInteger (TokenBundle.getCoin originalBundle)
            - coinToInteger (TokenBundle.getCoin mergedBundle)
            + coinToInteger (feeForOutput params originalBundle)
            - coinToInteger (feeForOutput params mergedBundle)
            - coinToInteger (feeForInput params)

    computeNewSize
        :: TokenBundle -> TokenBundle -> Either (SelectionError s) s
    computeNewSize originalBundle mergedBundle = guardSize params $ mconcat
        [ size selection
        , sizeOfInput params
        , sizeDistance
            (sizeOfOutput params originalBundle)
            (sizeOfOutput params mergedBundle)
        ]

    findFirstValidMergedBundle
        :: Either (SelectionError s) (Int, TokenBundle, TokenBundle)
    findFirstValidMergedBundle = maybeToEither SelectionAdaInsufficient
        $ outputs selection
        & NE.toList
        & zip [0 ..]
        & mapMaybe (\(i, b) -> (i, b, ) <$> mergeBundle b)
        & listToMaybe
      where
        mergeBundle :: TokenBundle -> Maybe TokenBundle
        mergeBundle outputBundle
            | Just mergedBundle <- computeMergedBundle
            , outputIsValid params mergedBundle =
                Just mergedBundle
            | otherwise =
                Nothing
          where
            computeMergedBundle :: Maybe TokenBundle
            computeMergedBundle = TokenBundle.setCoin mergedUnadjustedBundle
                <$> computeNewCoinValue

            computeNewCoinValue :: Maybe Coin
            computeNewCoinValue = coinFromInteger
                $ coinToInteger (TokenBundle.getCoin mergedUnadjustedBundle)
                - coinToInteger (feeForInput params)
                - coinToInteger (feeForOutput params mergedUnadjustedBundle)
                + coinToInteger (feeForOutput params outputBundle)

            mergedUnadjustedBundle :: TokenBundle
            mergedUnadjustedBundle = mconcat
                [ inputBundle
                , outputBundle
                , TokenBundle.fromCoin (feeExcess selection)
                ]

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
          let newFeeExcess = feeExcess selection <> Coin.distance
                  (feeForOutput params inputBundle)
                  (feeForOutput params outputBundle)
          pure
              $ selection {feeExcess = newFeeExcess, size = newSize}
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
    -> NonEmpty TokenBundle
    -> Maybe (NonEmpty TokenBundle)
reduceOutputAdaQuantities params reductionRequired bundles =
    NE.fromList <$> go reductionRequired [] (NE.toList bundles)
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

increaseOutputAdaQuantities
    :: Coin
    -> NonEmpty TokenBundle
    -> NonEmpty TokenBundle
increaseOutputAdaQuantities incrementRequired bundles =
    NE.zipWith (<>) bundleIncrements bundles
  where
    bundleIncrements = TokenBundle.fromCoin <$>
        Coin.equipartition incrementRequired bundles

coalesceTokenBundles
    :: SelectionParameters i
    -> NonEmpty TokenBundle
    -> NonEmpty TokenBundle
coalesceTokenBundles params bundles = coalesce bundles
  where
    coalesce (b :| []) =
        b :| []
    coalesce (b1 :| (b2 : bs)) | outputSizeWithinLimit params (b1 <> b2) =
        coalesce ((b1 <> b2) :| bs)
    coalesce (b1 :| (b2 : bs)) =
        b1 `NE.cons` coalesce (b2 :| bs)

addInput
    :: (i, TokenBundle)
    -> Selection i s
    -> Selection i s
addInput input selection = selection
    { inputs = NE.cons input (inputs selection) }

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
    { outputs = NE.fromList $ L.insertBy
        (outputOrdering params)
        (outputBundle)
        (NE.toList $ outputs selection)
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
guardE condition e = if condition then Right () else Left e

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

replaceHeadOfList :: NonEmpty a -> a -> NonEmpty a
replaceHeadOfList (_ :| as) a = a :| as

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
    | Just success <- maybesToMaybe (eitherToMaybe <$> eithers) =
        pure success
    | otherwise =
        NE.head eithers

maybesToMaybe :: NonEmpty (Maybe a) -> Maybe a
maybesToMaybe = listToMaybe . catMaybes . NE.toList
