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

    -- * Creating selections
      create
    , Selection (..)
    , SelectionError (..)
    , SelectionFullError (..)

    -- * Checking selections for correctness
    , SelectionCorrectness (..)
    , check

    -- * Classes
    , TxSize (..)

    ----------------------------------------------------------------------------
    -- Internal interface
    ----------------------------------------------------------------------------

    -- * Computing bulk properties of selections
    , computeCurrentFee
    , computeCurrentSize
    , computeMinimumFee

    -- * Minimizing fees
    , minimizeFee
    , minimizeFeeForOutput

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxConstraints (..), txOutputCoinCost, txOutputHasValidSize )
import Data.Bifunctor
    ( first )
import Data.Either.Extra
    ( eitherToMaybe, maybeToEither )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes, listToMaybe )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Selections
--------------------------------------------------------------------------------

data Selection i s = Selection
    { inputIds :: !(NonEmpty i)
      -- ^ The selected inputs.
    , inputBalance :: !TokenBundle
      -- ^ The total balance of value provided by the inputs.
    , outputs :: !(NonEmpty TokenBundle)
      -- ^ The outputs, adjusted to pay for the fee.
    , fee :: !Coin
      -- ^ The actual fee payable for this selection.
    , feeExcess :: !Coin
      -- ^ The excess over the minimum permissible fee for this selection.
    , size :: !s
      -- ^ The size of this selection.
    , rewardWithdrawal :: !Coin
      -- ^ The reward withdrawal amount, if any.
    }
    deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------
-- Selection correctness
--------------------------------------------------------------------------------

data SelectionCorrectness s
    = SelectionCorrect
    | SelectionAssetBalanceIncorrect
      SelectionAssetBalanceIncorrectError
    | SelectionFeeIncorrect
      SelectionFeeIncorrectError
    | SelectionFeeExcessIncorrect
      SelectionFeeExcessIncorrectError
    | SelectionFeeInsufficient
      SelectionFeeInsufficientError
    | SelectionOutputBelowMinimumAdaQuantity
      SelectionOutputBelowMinimumAdaQuantityError
    | SelectionOutputSizeExceedsLimit
      SelectionOutputSizeExceedsLimitError
    | SelectionSizeExceedsLimit
     (SelectionSizeExceedsLimitError s)
    | SelectionSizeIncorrect
     (SelectionSizeIncorrectError s)
    deriving (Eq, Show)

check
    :: TxSize s
    => TxConstraints s
    -> Selection i s
    -> SelectionCorrectness s
check constraints selection
    | Just e <- checkAssetBalance selection =
        SelectionAssetBalanceIncorrect e
    | Just e <- checkFee selection =
        SelectionFeeIncorrect e
    | Just e <- checkFeeSufficient constraints selection =
        SelectionFeeInsufficient e
    | Just e <- checkFeeExcess constraints selection =
        SelectionFeeExcessIncorrect e
    | Just e <- checkOutputMinimumAdaQuantities constraints selection =
        SelectionOutputBelowMinimumAdaQuantity e
    | Just e <- checkOutputSizes constraints selection =
        SelectionOutputSizeExceedsLimit e
    | Just e <- checkSizeWithinLimit constraints selection =
        SelectionSizeExceedsLimit e
    | Just e <- checkSizeCorrectness constraints selection =
        SelectionSizeIncorrect e
    | otherwise =
        SelectionCorrect

--------------------------------------------------------------------------------
-- Selection correctness: asset balance correctness
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
checkAssetBalance Selection {inputBalance, outputs}
    | assetBalanceInputs == assetBalanceOutputs =
        Nothing
    | otherwise =
        Just SelectionAssetBalanceIncorrectError
            { assetBalanceInputs
            , assetBalanceOutputs
            }
  where
    assetBalanceInputs = view #tokens inputBalance
    assetBalanceOutputs = F.foldMap (tokens) outputs

--------------------------------------------------------------------------------
-- Selection correctness: fee correctness
--------------------------------------------------------------------------------

data SelectionFeeIncorrectError = SelectionFeeIncorrectError
    { selectionFeeComputed
        :: Either NegativeCoin Coin
    , selectionFeeStored
        :: Coin
    }
    deriving (Eq, Show)

checkFee :: Selection i s -> Maybe SelectionFeeIncorrectError
checkFee selection =
    case computeCurrentFee selection of
      Left negativeFee ->
          pure SelectionFeeIncorrectError
              { selectionFeeComputed = Left negativeFee
              , selectionFeeStored = fee selection
              }
      Right positiveFee | positiveFee /= fee selection ->
          pure SelectionFeeIncorrectError
              { selectionFeeComputed = Right positiveFee
              , selectionFeeStored = fee selection
              }
      Right _ ->
          Nothing

--------------------------------------------------------------------------------
-- Selection correctness: fee excess correctness
--------------------------------------------------------------------------------

data SelectionFeeExcessIncorrectError = SelectionFeeExcessIncorrectError
    { selectionFeeExcessActual
        :: Coin
    , selectionFeeExcessExpected
        :: Coin
    }
    deriving (Eq, Show)

checkFeeExcess
    :: TxConstraints s
    -> Selection i s
    -> Maybe SelectionFeeExcessIncorrectError
checkFeeExcess constraints selection =
    check =<< eitherToMaybe (computeCurrentFee selection)
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
            (computeMinimumFee constraints selection)

--------------------------------------------------------------------------------
-- Selection correctness: fee sufficiency
--------------------------------------------------------------------------------

data SelectionFeeInsufficientError = SelectionFeeInsufficientError
    { selectionFeeActual
        :: Either NegativeCoin Coin
    , selectionFeeMinimum
        :: Coin
    }
    deriving (Eq, Show)

checkFeeSufficient
    :: TxConstraints s
    -> Selection i s
    -> Maybe SelectionFeeInsufficientError
checkFeeSufficient constraints selection =
    case computeCurrentFee selection of
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
    selectionFeeMinimum = computeMinimumFee constraints selection

--------------------------------------------------------------------------------
-- Selection correctness: minimum ada quantities
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
    :: TxConstraints s
    -> Selection i s
    -> Maybe SelectionOutputBelowMinimumAdaQuantityError
checkOutputMinimumAdaQuantities constraints selection =
     maybesToMaybe $ checkOutput <$> outputs selection
  where
    checkOutput
        :: TokenBundle
        -> Maybe SelectionOutputBelowMinimumAdaQuantityError
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
            txOutputMinimumAdaQuantity constraints (view #tokens outputBundle)

--------------------------------------------------------------------------------
-- Selection correctness: output sizes
--------------------------------------------------------------------------------

data SelectionOutputSizeExceedsLimitError = SelectionOutputSizeExceedsLimitError
    { selectionOutput :: TokenBundle }
    deriving (Eq, Show)

checkOutputSizes
    :: TxSize s
    => TxConstraints s
    -> Selection i s
    -> Maybe SelectionOutputSizeExceedsLimitError
checkOutputSizes constraints selection =
     maybesToMaybe $ checkOutput <$> outputs selection
  where
    checkOutput
        :: TokenBundle
        -> Maybe SelectionOutputSizeExceedsLimitError
    checkOutput selectionOutput
        | txOutputHasValidSize constraints selectionOutput =
            Nothing
        | otherwise =
            Just SelectionOutputSizeExceedsLimitError
                { selectionOutput }

--------------------------------------------------------------------------------
-- Selection correctness: selection size (in comparison to the stored value)
--------------------------------------------------------------------------------

data SelectionSizeIncorrectError s = SelectionSizeIncorrectError
    { selectionSizeComputed :: s
    , selectionSizeStored :: s
    }
    deriving (Eq, Show)

checkSizeCorrectness
    :: (Eq s, Monoid s)
    => TxConstraints s
    -> Selection i s
    -> Maybe (SelectionSizeIncorrectError s)
checkSizeCorrectness constraints selection
    | selectionSizeComputed == selectionSizeStored =
        Nothing
    | otherwise = pure SelectionSizeIncorrectError
        { selectionSizeComputed
        , selectionSizeStored
        }
  where
    selectionSizeComputed = computeCurrentSize constraints selection
    selectionSizeStored = size selection

--------------------------------------------------------------------------------
-- Selection correctness: selection size (in comparison to the limit)
--------------------------------------------------------------------------------

data SelectionSizeExceedsLimitError s = SelectionSizeExceedsLimitError
    { selectionSizeComputed :: s
    , selectionSizeMaximum :: s
    }
    deriving (Eq, Show)

checkSizeWithinLimit
    :: (Monoid s, Ord s)
    => TxConstraints s
    -> Selection i s
    -> Maybe (SelectionSizeExceedsLimitError s)
checkSizeWithinLimit constraints selection
    | selectionSizeComputed <= selectionSizeMaximum =
        Nothing
    | otherwise = pure SelectionSizeExceedsLimitError
        { selectionSizeComputed
        , selectionSizeMaximum
        }
  where
    selectionSizeComputed = computeCurrentSize constraints selection
    selectionSizeMaximum = txMaximumSize constraints

--------------------------------------------------------------------------------
-- Selection query functions
--------------------------------------------------------------------------------

-- | Calculates the current fee for a selection.
--
computeCurrentFee :: Selection i s -> Either NegativeCoin Coin
computeCurrentFee Selection {inputBalance, outputs, rewardWithdrawal}
    | adaBalanceIn >= adaBalanceOut =
        Right adaDifference
    | otherwise =
        Left (NegativeCoin adaDifference)
  where
    adaBalanceIn =
        rewardWithdrawal <> view #coin inputBalance
    adaBalanceOut =
        F.foldMap (TokenBundle.getCoin) outputs
    adaDifference =
        Coin.distance adaBalanceIn adaBalanceOut

-- | Calculates the current size of a selection.
--
computeCurrentSize
    :: Monoid s
    => TxConstraints s
    -> Selection i s
    -> s
computeCurrentSize constraints selection = mconcat
    [ txBaseSize constraints
    , F.foldMap (const $ txInputSize constraints) (inputIds selection)
    , F.foldMap (txOutputSize constraints) (outputs selection)
    , txRewardWithdrawalSize constraints (rewardWithdrawal selection)
    ]

-- | Calculates the minimum permissible fee for a selection.
--
computeMinimumFee :: TxConstraints s -> Selection i s -> Coin
computeMinimumFee constraints selection = mconcat
    [ txBaseCost constraints
    , F.foldMap (const $ txInputCost constraints) (inputIds selection)
    , F.foldMap (txOutputCost constraints) (outputs selection)
    , txRewardWithdrawalCost constraints (rewardWithdrawal selection)
    ]

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
-- Creating selections
--------------------------------------------------------------------------------

create
    :: forall i s. TxSize s
    => TxConstraints s
    -> Coin
    -- ^ Reward balance
    -> TokenBundle
    -- ^ Input balance
    -> NonEmpty i
    -- ^ Inputs
    -> NonEmpty TokenMap
    -- ^ Outputs
    -> Either (SelectionError s) (Selection i s)
create constraints rewardWithdrawal inputBalance inputIds outputs = do
    let minimizedOutputs = assignMinimumAdaQuantity constraints <$> outputs
    let unbalancedSelection = Selection
            { inputIds
            , inputBalance
            , outputs = minimizedOutputs
            , fee = Coin 0
            , feeExcess = Coin 0
            , size = mempty
            , rewardWithdrawal
            }
    unbalancedFee <- first (const SelectionAdaInsufficient) $
        computeCurrentFee unbalancedSelection
    let minimumFeeForUnbalancedSelection =
            computeMinimumFee constraints unbalancedSelection
    unbalancedFeeExcess <- maybeToEither SelectionAdaInsufficient $
            Coin.subtractCoin unbalancedFee minimumFeeForUnbalancedSelection
    let (minimizedFeeExcess, maximizedOutputs) =
            minimizeFee constraints (unbalancedFeeExcess, minimizedOutputs)
    let costIncrease = Coin.distance
            (totalCoinCost minimizedOutputs)
            (totalCoinCost maximizedOutputs)
    let balancedSelection = unbalancedSelection
            { fee = mconcat
                [ minimumFeeForUnbalancedSelection
                , minimizedFeeExcess
                , costIncrease
                ]
            , feeExcess = minimizedFeeExcess
            , outputs = maximizedOutputs
            }
    size <- guardSize constraints $
        computeCurrentSize constraints balancedSelection
    pure balancedSelection {size}
  where
    totalCoinCost :: NonEmpty TokenBundle -> Coin
    totalCoinCost = F.foldMap (txOutputCoinCost constraints . view #coin)

    assignMinimumAdaQuantity :: TxConstraints s -> TokenMap -> TokenBundle
    assignMinimumAdaQuantity constraints m =
        TokenBundle c m
      where
        c = txOutputMinimumAdaQuantity constraints m

--------------------------------------------------------------------------------
-- Minimizing fees
--------------------------------------------------------------------------------

minimizeFee
    :: TxConstraints s
    -> (Coin, NonEmpty TokenBundle)
    -- ^ Fee excess and output bundles.
    -> (Coin, NonEmpty TokenBundle)
    -- ^ Fee excess and output bundles after optimization.
minimizeFee constraints (currentFeeExcess, outputs) =
    NE.fromList <$> run currentFeeExcess (NE.toList outputs) []
  where
    run :: Coin -> [TokenBundle] -> [TokenBundle] -> (Coin, [TokenBundle])
    run (Coin 0) remaining processed =
        (Coin 0, L.reverse processed <> remaining)
    run feeExcessRemaining [] processed =
        (feeExcessRemaining, L.reverse processed)
    run feeExcessRemaining (output : remaining) processed =
        run feeExcessRemaining' remaining (output' : processed)
      where
        (feeExcessRemaining', output') =
            minimizeFeeForOutput constraints (feeExcessRemaining, output)

minimizeFeeForOutput
    :: TxConstraints s
    -> (Coin, TokenBundle)
    -- ^ Fee excess and output bundle.
    -> (Coin, TokenBundle)
    -- ^ Fee excess and output bundle after optimization.
minimizeFeeForOutput constraints =
    findFixedPoint reduceFee
  where
    reduceFee :: (Coin, TokenBundle) -> (Coin, TokenBundle)
    reduceFee (feeExcess, outputBundle)
        | outputCoinFinal > outputCoin &&
          outputCoinFinalCostIncrease < outputCoinFinalIncrease =
             (feeExcessFinal, outputBundleFinal)
        | otherwise =
            (feeExcess, outputBundle)
      where
        outputCoin = view #coin outputBundle
        outputCoinMaxCostIncrease = Coin.distance
            (txOutputCoinCost constraints outputCoin)
            (txOutputCoinCost constraints $ outputCoin <> feeExcess)
        outputCoinFinal = Coin
            $ unCoin outputCoin
            + unCoin feeExcess
            - unCoin outputCoinMaxCostIncrease
        outputCoinFinalCostIncrease = Coin.distance
            (txOutputCoinCost constraints outputCoin)
            (txOutputCoinCost constraints outputCoinFinal)
        outputCoinFinalIncrease = Coin.distance outputCoin outputCoinFinal
        outputBundleFinal = TokenBundle.setCoin outputBundle outputCoinFinal
        feeExcessFinal = Coin
            $ unCoin feeExcess
            - unCoin outputCoinFinalIncrease
            - unCoin outputCoinFinalCostIncrease

--------------------------------------------------------------------------------
-- Miscellaneous types and functions
--------------------------------------------------------------------------------

newtype NegativeCoin = NegativeCoin
    { unNegativeCoin :: Coin
    }
    deriving (Eq, Show)

class (Ord s, Monoid s) => TxSize s where
    txSizeDistance :: s -> s -> s

findFixedPoint :: Eq a => (a -> a) -> a -> a
findFixedPoint f = findInner
  where
    findInner a = let fa = f a in if a == fa then a else findInner fa

guardSize :: Ord s => TxConstraints s -> s -> Either (SelectionError s) s
guardSize constraints selectionSizeRequired
    | selectionSizeRequired <= selectionSizeMaximum =
        pure selectionSizeRequired
    | otherwise =
        Left $ SelectionFull SelectionFullError
            { selectionSizeMaximum
            , selectionSizeRequired
            }
  where
    selectionSizeMaximum = txMaximumSize constraints

maybesToMaybe :: NonEmpty (Maybe a) -> Maybe a
maybesToMaybe = listToMaybe . catMaybes . NE.toList
