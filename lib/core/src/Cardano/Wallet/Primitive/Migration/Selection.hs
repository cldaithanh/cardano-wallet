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

    -- * Selections
    , Selection (..)
    , SelectionError (..)
    , SelectionFullError (..)

    -- * Creating a selection
    , create

    ----------------------------------------------------------------------------
    -- Internal interface
    ----------------------------------------------------------------------------

    -- * Computing bulk properties of a selection
    , computeCurrentFee
    , computeCurrentSize
    , computeMinimumFee

    -- * Selection parameter functions
    , costOfOutputCoin
    , excessAdaForOutput
    , minimumAdaQuantityForOutputCoin
    , outputIsValid
    , outputSatisfiesMinimumAdaQuantity
    , outputSizeWithinLimit
    , sizeOfOutputCoin

    -- * Selection invariants
    , SelectionInvariantStatus (..)
    , checkInvariant

    -- * Selection queries
    , outputOrdering

    -- * Coalescing token bundles
    , coalesceOutputs

    -- * Minimizing fee excess
    , minimizeFeeExcess
    , minimizeFeeExcessForOutput

    -- * Miscellaneous functions
    , findFixedPoint

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), subtractCoin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Data.Either.Extra
    ( eitherToMaybe, maybeToEither )
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
    ( comparing )
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
    { costOfEmptySelection :: Coin
      -- ^ The constant cost of an empty selection.
    , costOfInput :: Coin
      -- ^ The constant cost of a selection input.
    , costOfOutput :: TokenBundle -> Coin
      -- ^ The variable cost of a selection output.
    , costOfRewardWithdrawal :: Coin -> Coin
      -- ^ The variable cost of a reward withdrawal.
    , sizeOfEmptySelection :: s
      -- ^ The constant size of an empty selection.
    , sizeOfInput :: s
      -- ^ The constant size of a selection input.
    , sizeOfOutput :: TokenBundle -> s
      -- ^ The variable size of a selection output.
    , sizeOfRewardWithdrawal :: Coin -> s
      -- ^ The variable size of a reward withdrawal.
    , maximumSizeOfOutput :: s
      -- ^ The maximum size of a selection output.
    , maximumSizeOfSelection :: s
      -- ^ The maximum size of a selection.
    , maximumTokenQuantity :: TokenQuantity
      -- ^ The maximum token quantity of an output.
    , minimumAdaQuantityForOutput :: TokenMap -> Coin
      -- ^ The variable minimum ada quantity for an output.
    }

--------------------------------------------------------------------------------
-- Selection parameter functions
--------------------------------------------------------------------------------

costOfOutputCoin :: SelectionParameters s -> Coin -> Coin
costOfOutputCoin params = costOfOutput params . TokenBundle.fromCoin

excessAdaForOutput :: SelectionParameters s -> TokenBundle -> Coin
excessAdaForOutput params bundle =
    fromMaybe (Coin 0) result
  where
    result = subtractCoin
        (view #coin bundle)
        (minimumAdaQuantityForOutput params $ view #tokens bundle)

minimumAdaQuantityForOutputCoin :: SelectionParameters s -> Coin
minimumAdaQuantityForOutputCoin =
    flip minimumAdaQuantityForOutput TokenMap.empty

sizeOfOutputCoin :: SelectionParameters s -> Coin -> s
sizeOfOutputCoin params = sizeOfOutput params . TokenBundle.fromCoin

outputIsValid
    :: forall s. Size s
    => SelectionParameters s
    -> TokenBundle
    -> Bool
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

outputSizeWithinLimit :: Size s => SelectionParameters s -> TokenBundle -> Bool
outputSizeWithinLimit params b =
    sizeOfOutput params b <= maximumSizeOfOutput params

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
    :: Size s
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
    check =<< eitherToMaybe (computeCurrentFee selection)
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
            (computeMinimumFee params selection)

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
    case computeCurrentFee selection of
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
    selectionFeeMinimum = computeMinimumFee params selection

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
    :: Size s
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
    selectionSizeComputed = computeCurrentSize params selection
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
    selectionSizeComputed = computeCurrentSize params selection
    selectionSizeMaximum = maximumSizeOfSelection params

--------------------------------------------------------------------------------
-- Selection query functions
--------------------------------------------------------------------------------

-- | Calculates the current fee for a selection.
--
computeCurrentFee :: Selection i s -> Either NegativeCoin Coin
computeCurrentFee Selection {inputs, outputs, rewardWithdrawal}
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

computeFeeExcess
    :: SelectionParameters s -> Selection i s -> Maybe Coin
computeFeeExcess params selection = case computeCurrentFee selection of
    Right currentFee | currentFee >= minimumFee ->
        Just $ Coin.distance currentFee minimumFee
    _ ->
        Nothing
  where
    minimumFee = computeMinimumFee params selection

-- | Calculates the current size of a selection.
--
computeCurrentSize
    :: Monoid s
    => SelectionParameters s
    -> Selection i s
    -> s
computeCurrentSize params selection = mconcat
    [ sizeOfEmptySelection params
    , F.foldMap (const $ sizeOfInput params) (inputs selection)
    , F.foldMap (sizeOfOutput params) (outputs selection)
    , sizeOfRewardWithdrawal params (rewardWithdrawal selection)
    ]

-- | Calculates the minimum permissible fee for a selection.
--
computeMinimumFee :: SelectionParameters s -> Selection i s -> Coin
computeMinimumFee params selection = mconcat
    [ costOfEmptySelection params
    , F.foldMap (const $ costOfInput params) (inputs selection)
    , F.foldMap (costOfOutput params) (outputs selection)
    , costOfRewardWithdrawal params (rewardWithdrawal selection)
    ]

-- | Defines the correct ordering of outputs in a selection.
--
outputOrdering
    :: SelectionParameters s
    -> TokenBundle
    -> TokenBundle
    -> Ordering
outputOrdering params =
    comparing (minimumAdaQuantityForOutput params . view #tokens)

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
-- Creating a selection
--------------------------------------------------------------------------------

create
    :: forall i s. Size s
    => SelectionParameters s
    -> Coin
    -> NonEmpty (i, TokenBundle)
    -> Either (SelectionError s) (Selection i s)
create params rewardWithdrawal inputs = do
    let minimizedOutputs = minimizeOutput params <$> NE.sortBy
            (outputOrdering params)
            (coalesceOutputs params $ snd <$> inputs)
    let unbalancedSelection = Selection
            { inputs
            , outputs = minimizedOutputs
            , feeExcess = Coin 0
            , size = mempty
            , rewardWithdrawal
            }
    currentFeeExcess <- maybeToEither SelectionAdaInsufficient $
        computeFeeExcess params unbalancedSelection
    let (feeExcess, outputs) =
            minimizeFeeExcess params (currentFeeExcess, minimizedOutputs)
    let balancedSelection = unbalancedSelection {feeExcess, outputs}
    size <- guardSize params $ computeCurrentSize params balancedSelection
    pure balancedSelection {size}

--------------------------------------------------------------------------------
-- Coalescing outputs
--------------------------------------------------------------------------------

coalesceOutputs
    :: Size s
    => SelectionParameters s
    -> NonEmpty TokenBundle
    -> NonEmpty TokenBundle
coalesceOutputs params = splitBundleIfLimitsExceeded params . F.fold

minimizeOutput :: SelectionParameters s -> TokenBundle -> TokenBundle
minimizeOutput params output
    = TokenBundle.setCoin output
    $ minimumAdaQuantityForOutput params (view #tokens output)

--------------------------------------------------------------------------------
-- Minimizing the fee excess
--------------------------------------------------------------------------------

minimizeFeeExcess
    :: SelectionParameters s
    -> (Coin, NonEmpty TokenBundle)
    -- ^ Fee excess and output bundles.
    -> (Coin, NonEmpty TokenBundle)
    -- ^ Fee excess and output bundles after optimization.
minimizeFeeExcess params (currentFeeExcess, outputs) =
    NE.fromList <$> run currentFeeExcess (NE.toList outputs) []
  where
    run :: Coin -> [TokenBundle] -> [TokenBundle] -> (Coin, [TokenBundle])
    run (Coin 0) remaining processed =
        (Coin 0, insertManyBy (outputOrdering params) remaining processed)
    run feeExcessRemaining [] processed =
        (feeExcessRemaining, L.sortBy (outputOrdering params) processed)
    run feeExcessRemaining (output : remaining) processed =
        run feeExcessRemaining' remaining (output' : processed)
      where
        (feeExcessRemaining', output') =
            minimizeFeeExcessForOutput params (feeExcessRemaining, output)

minimizeFeeExcessForOutput
    :: SelectionParameters s
    -> (Coin, TokenBundle)
    -- ^ Fee excess and output bundle.
    -> (Coin, TokenBundle)
    -- ^ Fee excess and output bundle after optimization.
minimizeFeeExcessForOutput params =
    findFixedPoint reduceFeeExcess
  where
    reduceFeeExcess :: (Coin, TokenBundle) -> (Coin, TokenBundle)
    reduceFeeExcess (feeExcess, outputBundle) =
        (feeExcessFinal, TokenBundle.setCoin outputBundle outputCoinFinal)
      where
        outputCoin = view #coin outputBundle
        outputCoinMaxCostIncrease = Coin.distance
            (costOfOutputCoin params outputCoin)
            (costOfOutputCoin params $ outputCoin <> feeExcess)
        outputCoinFinal = Coin
            $ unCoin outputCoin
            + unCoin feeExcess
            - unCoin outputCoinMaxCostIncrease
        outputCoinFinalCostIncrease = Coin.distance
            (costOfOutputCoin params outputCoin)
            (costOfOutputCoin params outputCoinFinal)
        outputCoinFinalIncrease = Coin.distance outputCoin outputCoinFinal
        feeExcessFinal = Coin
            $ unCoin feeExcess
            - unCoin outputCoinFinalIncrease
            - unCoin outputCoinFinalCostIncrease

--------------------------------------------------------------------------------
-- Splitting bundles
--------------------------------------------------------------------------------

splitBundleIfLimitsExceeded
    :: Size s
    => SelectionParameters s
    -> TokenBundle
    -> NonEmpty TokenBundle
splitBundleIfLimitsExceeded params b
    = splitBundlesWithExcessiveTokenQuantities params
    $ splitBundlesWithExcessiveSizes params
    $ b :| []

splitBundlesWithExcessiveSizes
    :: Size s
    => SelectionParameters s
    -> NonEmpty TokenBundle
    -> NonEmpty TokenBundle
splitBundlesWithExcessiveSizes params bs =
    splitBundleIfSizeExceedsLimit params =<< bs

splitBundlesWithExcessiveTokenQuantities
    :: SelectionParameters s
    -> NonEmpty TokenBundle
    -> NonEmpty TokenBundle
splitBundlesWithExcessiveTokenQuantities params bs =
    (`TokenBundle.equipartitionQuantitiesWithUpperBound` maxQuantity) =<< bs
  where
    maxQuantity = maximumTokenQuantity params

splitBundleIfSizeExceedsLimit
    :: Size s
    => SelectionParameters s
    -> TokenBundle
    -> NonEmpty TokenBundle
splitBundleIfSizeExceedsLimit params bundle
    | outputSizeWithinLimit params bundleWithMaxAda =
        pure bundle
    | otherwise =
        splitInHalf bundle >>= splitBundleIfSizeExceedsLimit params
    | otherwise =
        pure bundle
  where
    splitInHalf = flip TokenBundle.equipartitionAssets (() :| [()])
    bundleWithMaxAda = TokenBundle.setCoin bundle maxBound

--------------------------------------------------------------------------------
-- Miscellaneous types and functions
--------------------------------------------------------------------------------

newtype NegativeCoin = NegativeCoin
    { unNegativeCoin :: Coin
    }
    deriving (Eq, Show)

class (Ord a, Monoid a) => Size a where
    sizeDistance :: a -> a -> a

findFixedPoint :: Eq a => (a -> a) -> a -> a
findFixedPoint f = findInner
  where
    findInner a = let fa = f a in if a == fa then a else findInner fa

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

insertManyBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
insertManyBy = L.foldl' . flip . L.insertBy

maybesToMaybe :: NonEmpty (Maybe a) -> Maybe a
maybesToMaybe = listToMaybe . catMaybes . NE.toList
