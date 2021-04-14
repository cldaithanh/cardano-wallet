{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Migration
    (
      -- * UTxO entry category
      categorizeUTxO
    , categorizeUTxOEntries
    , CategorizedUTxO (..)

      -- * Migration plans
    , createPlan
    , MigrationPlan (..)

      -- * Utility functions
    , addValueToOutputs
    , selectWithPriority

    ) where

import Prelude

import Cardano.Wallet.Primitive.Migration.Selection
    ( Selection (..), SelectionError (..), TxSize (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxConstraints (..), TxIn, TxOut, txOutputIsValid, txOutputHasValidSize )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Monad
    ( (>=>) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes, listToMaybe )

import qualified Cardano.Wallet.Primitive.Migration.Selection as Selection
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Migration
--------------------------------------------------------------------------------

data MigrationPlan i s = MigrationPlan
    { selections :: [Selection i s]
    , unselected :: [(i, TokenBundle)]
    , totalFee :: Coin
    }
    deriving (Eq, Show)

createPlan
    :: TxSize s
    => TxConstraints s
    -> [(i, TokenBundle)]
    -> Coin
    -- ^ Reward balance
    -> MigrationPlan i s
createPlan constraints entries =
    run [] (categorizeUTxOEntries constraints entries)
  where
    run selections utxo rewardBalance =
        case createSelection constraints utxo rewardBalance of
            Just (utxo', selection) ->
                run (selection : selections) utxo' (Coin 0)
            Nothing -> MigrationPlan
                { selections
                , unselected = uncategorizeUTxOEntries utxo
                , totalFee = F.foldMap (view #fee) selections
                }

createSelection
    :: TxSize s
    => TxConstraints s
    -> CategorizedUTxO i
    -> Coin
    -- ^ Reward balance
    -> Maybe (CategorizedUTxO i, Selection i s)
createSelection constraints utxo rewardBalance =
    initializeSelection constraints utxo rewardBalance
    <&> extendSelection constraints

initializeSelection
    :: TxSize s
    => TxConstraints s
    -> CategorizedUTxO i
    -> Coin
    -- ^ Reward balance
    -> Maybe (CategorizedUTxO i, Selection i s)
initializeSelection constraints utxoAtStart rewardBalance
    | Just (supporter, utxo) <- utxoAtStart `select` Supporter =
        run utxo (supporter :| [])
    | otherwise =
        Nothing
  where
    run utxo inputs =
        case Selection.create constraints rewardBalance inputs of
            Right selection ->
                Just (utxo, selection)
            Left SelectionAdaInsufficient ->
                case utxo `select` Supporter of
                    Just (input, utxo') ->
                        run utxo' (input `NE.cons` inputs)
                    Nothing ->
                        Nothing
            Left (SelectionFull _) ->
                Nothing

extendSelection
    :: TxSize s
    => TxConstraints s
    -> (CategorizedUTxO i, Selection i s)
    -> (CategorizedUTxO i, Selection i s)
extendSelection constraints = extendWithFreerider
  where
    extendWithFreerider (utxo, selection) =
        case extendWith Freerider constraints (utxo, selection) of
            Right (utxo', selection') ->
                extendWithFreerider (utxo', selection')
            Left ExtendSelectionAdaInsufficient ->
                extendWithSupporter (utxo, selection)
            Left ExtendSelectionEntriesExhausted ->
                extendWithSupporter (utxo, selection)
            Left ExtendSelectionFull ->
                (utxo, selection)

    extendWithSupporter (utxo, selection) =
        case extendWith Supporter constraints (utxo, selection) of
            Right (utxo', selection') ->
                extendWithFreerider (utxo', selection')
            Left ExtendSelectionAdaInsufficient ->
                extendWithInitiator (utxo, selection)
            Left ExtendSelectionEntriesExhausted ->
                extendWithInitiator (utxo, selection)
            Left ExtendSelectionFull ->
                (utxo, selection)

    extendWithInitiator (utxo, selection) =
        case extendWith Initiator constraints (utxo, selection) of
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
        Just (input, utxo') ->
            let inputs' = input `NE.cons` inputs selection in
            case Selection.create constraints (Coin 0) inputs' of
                Right selection' ->
                    Right (utxo', selection')
                Left SelectionAdaInsufficient ->
                    Left ExtendSelectionAdaInsufficient
                Left SelectionFull {} ->
                    Left ExtendSelectionFull
        Nothing ->
            Left ExtendSelectionEntriesExhausted

selectWithPriority
    :: CategorizedUTxO i
    -> NonEmpty UTxOEntryCategory
    -> Maybe ((i, TokenBundle), CategorizedUTxO i)
selectWithPriority utxo = maybesToMaybe . fmap (select utxo)

select
    :: CategorizedUTxO i
    -> UTxOEntryCategory
    -> Maybe ((i, TokenBundle), CategorizedUTxO i)
select utxo = \case
    Initiator -> selectInitiator
    Supporter -> selectSupporter
    Freerider -> selectFreerider
    Ignorable -> selectIgnorable
  where
    selectInitiator = case initiators utxo of
        entry : remaining -> Just (entry, utxo {initiators = remaining})
        [] -> Nothing
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

data CategorizedUTxO i = CategorizedUTxO
    { initiators :: [(i, TokenBundle)]
    , supporters :: [(i, TokenBundle)]
    , freeriders :: [(i, TokenBundle)]
    , ignorables :: [(i, TokenBundle)]
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
    { initiators = entriesMatching Initiator
    , supporters = entriesMatching Supporter
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

data UTxOEntryCategory
    = Initiator
    -- ^ A coin or bundle that is capable of paying for its own marginal fee
    -- and the base transaction fee.
    | Supporter
    -- ^ A coin or bundle that is capable of paying for its own marginal fee,
    -- but not the base transaction fee.
    | Freerider
    -- ^ A bundle that is not capable of paying for its own marginal fee.
    | Ignorable
    -- ^ A coin that should not be added to a selection, because its value is
    -- lower than the marginal fee for an input.
    deriving (Eq, Show)

categorizeUTxOEntry
    :: TxSize s
    => TxConstraints s
    -> TokenBundle
    -> UTxOEntryCategory
categorizeUTxOEntry constraints b
    | Just c <- TokenBundle.toCoin b, coinIsIgnorable c =
        Ignorable
    | bundleIsInitiator b =
        Initiator
    | bundleIsSupporter b =
        Supporter
    | otherwise =
        Freerider
  where
    bundleIsInitiator :: TokenBundle -> Bool
    bundleIsInitiator b@(TokenBundle c m) = c >= mconcat
        [ txBaseCost constraints
        , txInputCost constraints
        , txOutputCost constraints b
        , txOutputMinimumAdaQuantity constraints m
        ]

    bundleIsSupporter :: TokenBundle -> Bool
    bundleIsSupporter b@(TokenBundle c m) = c >= mconcat
        [ txInputCost constraints
        , txOutputCost constraints b
        , txOutputMinimumAdaQuantity constraints m
        ]

    coinIsIgnorable :: Coin -> Bool
    coinIsIgnorable c = c <= txInputCost constraints

uncategorizeUTxOEntries :: CategorizedUTxO i -> [(i, TokenBundle)]
uncategorizeUTxOEntries utxo = mconcat
    [ initiators utxo
    , supporters utxo
    , freeriders utxo
    , ignorables utxo
    ]

--------------------------------------------------------------------------------
-- Adding value to outputs
--------------------------------------------------------------------------------

addValueToOutputs
    :: TxSize s
    => TxConstraints s
    -> TokenBundle
    -- ^ Value to add
    -> NonEmpty TokenBundle
    -- ^ Existing set of outputs
    -> NonEmpty TokenBundle
    -- ^ Set of outputs with the value added
addValueToOutputs constraints valueUnchecked outputs =
    F.foldl' (flip addToOutputs) outputs $
        splitOutputIfLimitsExceeded constraints valueUnchecked
  where
    addToOutputs :: TokenBundle -> NonEmpty TokenBundle -> NonEmpty TokenBundle
    addToOutputs value = NE.fromList . add [] . NE.toList
      where
        add considered (candidate : unconsidered) =
            case safeMergeOutputValue value candidate of
                Just merged ->
                    merged : (considered <> unconsidered)
                Nothing ->
                    add unconsidered (candidate : considered)
        add considered [] =
            value : considered

    safeMergeOutputValue :: TokenBundle -> TokenBundle -> Maybe TokenBundle
    safeMergeOutputValue a b
        | txOutputIsValid constraints valueWithMaxAda =
            Just value
        | otherwise =
            Nothing
      where
        value = a <> b
        valueWithMaxAda = TokenBundle.setCoin value maxBound

--------------------------------------------------------------------------------
-- Splitting output values
--------------------------------------------------------------------------------

splitOutputIfLimitsExceeded
    :: TxSize s
    => TxConstraints s
    -> TokenBundle
    -> NonEmpty TokenBundle
splitOutputIfLimitsExceeded constraints =
    splitOutputIfSizeExceedsLimit constraints >=>
    splitOutputIfTokenQuantityExceedsLimit constraints

splitOutputIfTokenQuantityExceedsLimit
    :: TxConstraints s
    -> TokenBundle
    -> NonEmpty TokenBundle
splitOutputIfTokenQuantityExceedsLimit
    = flip TokenBundle.equipartitionQuantitiesWithUpperBound
    . txOutputMaximumTokenQuantity

splitOutputIfSizeExceedsLimit
    :: TxSize s
    => TxConstraints s
    -> TokenBundle
    -> NonEmpty TokenBundle
splitOutputIfSizeExceedsLimit constraints bundle
    | txOutputHasValidSize constraints bundleWithMaxAda =
        pure bundle
    | otherwise =
        splitInHalf bundle >>= splitOutputIfSizeExceedsLimit constraints
    | otherwise =
        pure bundle
  where
    bundleWithMaxAda = TokenBundle.setCoin bundle maxBound
    splitInHalf = flip TokenBundle.equipartitionAssets (() :| [()])

--------------------------------------------------------------------------------
-- Miscellaneous types and functions
--------------------------------------------------------------------------------

maybesToMaybe :: NonEmpty (Maybe a) -> Maybe a
maybesToMaybe = listToMaybe . catMaybes . NE.toList
