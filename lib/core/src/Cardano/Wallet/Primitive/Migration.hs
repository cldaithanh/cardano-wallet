{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Migration
    (
      -- * Migration planning
      createPlan
    , MigrationPlan (..)
    , RewardBalance (..)

      -- * UTxO entry categorization
    , CategorizedUTxO (..)
    , UTxOEntryCategory (..)
    , categorizeUTxO
    , categorizeUTxOEntries
    , categorizeUTxOEntry
    , uncategorizeUTxO
    , uncategorizeUTxOEntries

      -- * Adding value to outputs
    , addValueToOutputs

      -- * Splitting outputs
    , splitOutputIfLimitsExceeded

    ) where

import Prelude

import Cardano.Wallet.Primitive.Migration.Selection
    ( Selection (..), SelectionError (..), TxSize (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxConstraints (..)
    , TxIn
    , TxOut
    , txOutputHasValidSize
    , txOutputHasValidTokenQuantities
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Monad
    ( (>=>) )
import Data.Either
    ( isRight )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )

import qualified Cardano.Wallet.Primitive.Migration.Selection as Selection
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Migration
--------------------------------------------------------------------------------

data MigrationPlan i s = MigrationPlan
    { selections :: ![Selection i s]
    , unselected :: !(CategorizedUTxO i)
    , totalFee :: !Coin
    }
    deriving (Eq, Show)

newtype RewardBalance = RewardBalance
    { unRewardBalance :: Coin }

createPlan
    :: TxSize s
    => TxConstraints s
    -> CategorizedUTxO i
    -> RewardBalance
    -> MigrationPlan i s
createPlan constraints =
    run []
  where
    run !selections !utxo !reward =
        case createSelection constraints utxo reward of
            Just (utxo', selection) ->
                run (selection : selections) utxo' (RewardBalance $ Coin 0)
            Nothing -> MigrationPlan
                { selections
                , unselected = utxo
                , totalFee = F.foldMap (view #fee) selections
                }

createSelection
    :: TxSize s
    => TxConstraints s
    -> CategorizedUTxO i
    -> RewardBalance
    -> Maybe (CategorizedUTxO i, Selection i s)
createSelection constraints utxo rewardWithdrawal =
    initializeSelection constraints utxo rewardWithdrawal
    <&> extendSelection constraints

initializeSelection
    :: forall i s. TxSize s
    => TxConstraints s
    -> CategorizedUTxO i
    -> RewardBalance
    -> Maybe (CategorizedUTxO i, Selection i s)
initializeSelection constraints utxoAtStart (RewardBalance reward) =
    initializeWith =<< utxoAtStart `select` Supporter
  where
    initializeWith ((inputId, inputValue), utxo) =
        case selectionResult of
            Right selection -> Just (utxo, selection)
            Left _ -> Nothing
      where
        selectionResult =
            Selection.create constraints reward inputValue [inputId] outputs
        outputs =
            addValueToOutputs constraints [] (view #tokens inputValue)

extendSelection
    :: TxSize s
    => TxConstraints s
    -> (CategorizedUTxO i, Selection i s)
    -> (CategorizedUTxO i, Selection i s)
extendSelection constraints = extendWithFreerider
  where
    extendWithFreerider (!utxo, !selection) =
        case extendWith Freerider constraints (utxo, selection) of
            Right (utxo', selection') ->
                extendWithFreerider (utxo', selection')
            Left ExtendSelectionAdaInsufficient ->
                extendWithSupporter (utxo, selection)
            Left ExtendSelectionEntriesExhausted ->
                extendWithSupporter (utxo, selection)
            Left ExtendSelectionFull ->
                (utxo, selection)

    extendWithSupporter (!utxo, !selection) =
        case extendWith Supporter constraints (utxo, selection) of
            Right (utxo', selection') ->
                extendWithFreerider (utxo', selection')
            Left ExtendSelectionAdaInsufficient ->
                (utxo, selection)
            Left ExtendSelectionEntriesExhausted ->
                (utxo, selection)
            Left ExtendSelectionFull ->
                (utxo, selection)

data ExtendSelectionError
    = ExtendSelectionAdaInsufficient
    | ExtendSelectionEntriesExhausted
    | ExtendSelectionFull

extendWith
    :: TxSize s
    => UTxOEntryCategory
    -> TxConstraints s
    -> (CategorizedUTxO i, Selection i s)
    -> Either ExtendSelectionError (CategorizedUTxO i, Selection i s)
extendWith category constraints (utxo, selection) =
    case utxo `select` category of
        Just ((inputId, inputValue), utxo') ->
            let inputIds' = inputId `NE.cons` inputIds selection in
            let inputBalance' = inputValue <> inputBalance selection in
            let outputs' = addValueToOutputs constraints
                  (view #tokens <$> F.toList (outputs selection))
                  (view #tokens inputValue) in
            let selectionResult = Selection.create
                    constraints (Coin 0) inputBalance' inputIds' outputs' in
            case selectionResult of
                Right selection' ->
                    Right (utxo', selection')
                Left SelectionAdaInsufficient ->
                    Left ExtendSelectionAdaInsufficient
                Left SelectionFull {} ->
                    Left ExtendSelectionFull
        Nothing ->
            Left ExtendSelectionEntriesExhausted

select
    :: CategorizedUTxO i
    -> UTxOEntryCategory
    -> Maybe ((i, TokenBundle), CategorizedUTxO i)
select utxo = \case
    Supporter -> selectSupporter
    Freerider -> selectFreerider
    Ignorable -> selectIgnorable
  where
    selectSupporter = case supporters utxo of
        entry : remaining -> Just (entry, utxo {supporters = remaining})
        [] -> Nothing
    selectFreerider = case freeriders utxo of
        entry : remaining -> Just (entry, utxo {freeriders = remaining})
        [] ->  Nothing
    selectIgnorable =
        -- We never select an entry that should be ignored:
        Nothing

--------------------------------------------------------------------------------
-- Categorization of UTxO entries
--------------------------------------------------------------------------------

data UTxOEntryCategory
    = Supporter
    -- ^ A coin or bundle that is capable of paying for its own marginal fee
    -- and the base transaction fee.
    | Freerider
    -- ^ A bundle that is not capable of paying for its own marginal fee.
    | Ignorable
    -- ^ A coin that should not be added to a selection, because its value is
    -- lower than the marginal fee for an input.
    deriving (Eq, Show)

data CategorizedUTxO i = CategorizedUTxO
    { supporters :: ![(i, TokenBundle)]
    , freeriders :: ![(i, TokenBundle)]
    , ignorables :: ![(i, TokenBundle)]
    }
    deriving (Eq, Show)

categorizeUTxO
    :: TxSize s
    => TxConstraints s
    -> UTxO
    -> CategorizedUTxO (TxIn, TxOut)
categorizeUTxO constraints (UTxO u) = categorizeUTxOEntries constraints $
    (\(i, o) -> ((i, o), view #tokens o)) <$> Map.toList u

categorizeUTxOEntries
    :: forall i s. TxSize s
    => TxConstraints s
    -> [(i, TokenBundle)]
    -> CategorizedUTxO i
categorizeUTxOEntries constraints uncategorizedEntries = CategorizedUTxO
    { supporters = entriesMatching Supporter
    , freeriders = entriesMatching Freerider
    , ignorables = entriesMatching Ignorable
    }
  where
    categorizedEntries :: [(i, (TokenBundle, UTxOEntryCategory))]
    categorizedEntries = uncategorizedEntries
        <&> (\(i, b) -> (i, (b, categorizeUTxOEntry constraints b)))

    entriesMatching :: UTxOEntryCategory -> [(i, TokenBundle)]
    entriesMatching category =
        fmap fst <$> L.filter ((== category) . snd . snd) categorizedEntries

categorizeUTxOEntry
    :: TxSize s
    => TxConstraints s
    -> TokenBundle
    -> UTxOEntryCategory
categorizeUTxOEntry constraints b
    | Just c <- TokenBundle.toCoin b, coinIsIgnorable c =
        Ignorable
    | bundleIsSupporter b =
        Supporter
    | otherwise =
        Freerider
  where
    bundleIsSupporter :: TokenBundle -> Bool
    bundleIsSupporter b =
        isRight $ Selection.create constraints (Coin 0) b [()] [view #tokens b]
        -- Note: this should be equivalent to:
        --
        -- c >= mconcat
        --     [ txBaseCost constraints
        --     , txInputCost constraints
        --     , txOutputCost constraints b
        --     , txOutputMinimumAdaQuantity constraints m
        --     ]

    coinIsIgnorable :: Coin -> Bool
    coinIsIgnorable c = c <= txInputCost constraints

uncategorizeUTxO :: CategorizedUTxO (TxIn, TxOut) -> UTxO
uncategorizeUTxO = UTxO . Map.fromList . fmap fst . uncategorizeUTxOEntries

uncategorizeUTxOEntries :: CategorizedUTxO i -> [(i, TokenBundle)]
uncategorizeUTxOEntries utxo = mconcat
    [ supporters utxo
    , freeriders utxo
    , ignorables utxo
    ]

--------------------------------------------------------------------------------
-- Adding value to outputs
--------------------------------------------------------------------------------

addValueToOutputs
    :: TxSize s
    => TxConstraints s
    -> [TokenMap]
    -- ^ Outputs
    -> TokenMap
    -- ^ Value to add
    -> NonEmpty TokenMap
    -- ^ Outputs with the value added
addValueToOutputs constraints outputs = NE.fromList
    . F.foldl' (flip add) outputs
    . splitOutputIfLimitsExceeded constraints
  where
    add :: TokenMap -> [TokenMap] -> [TokenMap]
    add value = run []
      where
        run :: [TokenMap] -> [TokenMap] -> [TokenMap]
        run considered (candidate : unconsidered) =
            case safeMerge value candidate of
                Just merged -> merged : (considered <> unconsidered)
                Nothing -> run (candidate : considered) unconsidered
        run considered [] =
            value : considered

    safeMerge :: TokenMap -> TokenMap -> Maybe TokenMap
    safeMerge a b
        | isSafe = Just value
        | otherwise = Nothing
      where
        isSafe = (&&)
            (txOutputHasValidSize constraints (TokenBundle maxBound value))
            (txOutputHasValidTokenQuantities constraints value)
        value = a <> b

--------------------------------------------------------------------------------
-- Splitting output values
--------------------------------------------------------------------------------

splitOutputIfLimitsExceeded
    :: TxSize s
    => TxConstraints s
    -> TokenMap
    -> NonEmpty TokenMap
splitOutputIfLimitsExceeded constraints =
    splitOutputIfTokenQuantityExceedsLimit constraints >=>
    splitOutputIfSizeExceedsLimit constraints

splitOutputIfSizeExceedsLimit
    :: TxSize s
    => TxConstraints s
    -> TokenMap
    -> NonEmpty TokenMap
splitOutputIfSizeExceedsLimit constraints value
    | txOutputHasValidSize constraints (TokenBundle maxBound value) =
        pure value
    | otherwise =
        split value >>= splitOutputIfSizeExceedsLimit constraints
    | otherwise =
        pure value
  where
    split = flip TokenMap.equipartitionAssets (() :| [(), (), ()])

splitOutputIfTokenQuantityExceedsLimit
    :: TxConstraints s
    -> TokenMap
    -> NonEmpty TokenMap
splitOutputIfTokenQuantityExceedsLimit
    = flip TokenMap.equipartitionQuantitiesWithUpperBound
    . txOutputMaximumTokenQuantity
