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
      MigrationPlan (..)
    , createPlan

      -- * UTxO entry categorization
    , CategorizedUTxO (..)
    , UTxOEntryCategory (..)
    , categorizeUTxO
    , categorizeUTxOEntries
    , categorizeUTxOEntry
    , uncategorizeUTxO
    , uncategorizeUTxOEntries

      -- * Utility functions
    , addValueToOutputs
    , splitOutputIfLimitsExceeded

    ) where

import Prelude

import Cardano.Wallet.Primitive.Migration.Selection
    ( Selection (..), SelectionError (..), TxSize (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
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
    , unselected :: CategorizedUTxO i
    , totalFee :: Coin
    }
    deriving (Eq, Show)

createPlan
    :: TxSize s
    => TxConstraints s
    -> CategorizedUTxO i
    -> Coin
    -- ^ Reward balance
    -> MigrationPlan i s
createPlan constraints =
    run []
  where
    run selections utxo rewardBalance =
        case createSelection constraints utxo rewardBalance of
            Just (utxo', selection) ->
                run (selection : selections) utxo' (Coin 0)
            Nothing -> MigrationPlan
                { selections
                , unselected = utxo
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
    :: forall i s. TxSize s
    => TxConstraints s
    -> CategorizedUTxO i
    -> Coin
    -- ^ Reward balance
    -> Maybe (CategorizedUTxO i, Selection i s)
initializeSelection constraints utxoAtStart rewardBalance =
    maybesToMaybe $ tryWith <$> tails [Freerider, Supporter, Initiator]
  where
    tryWith
        :: NonEmpty UTxOEntryCategory
        -> Maybe (CategorizedUTxO i, Selection i s)
    tryWith categories
        | Just ((inputId, inputValue), utxo) <-
            utxoAtStart `selectWithPriority` categories =
                run utxo inputValue [inputId]
        | otherwise =
            Nothing
      where
        run :: CategorizedUTxO i
            -> TokenBundle
            -> NonEmpty i
            -> Maybe (CategorizedUTxO i, Selection i s)
        run utxo inputBalance inputIds =
            let selectionResult = Selection.create
                    constraints rewardBalance inputBalance inputIds in
            case selectionResult of
                Right selection ->
                    Just (utxo, selection)
                Left SelectionAdaInsufficient ->
                    case utxo `selectWithPriority` categories of
                        Just ((inputId, inputValue), utxo') ->
                            run utxo'
                                (inputValue <> inputBalance)
                                (inputId `NE.cons` inputIds)
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
        Just ((inputId, inputValue), utxo') ->
            let inputIds' = inputId `NE.cons` inputIds selection in
            let inputBalance' = inputValue <> inputBalance selection in
            case Selection.create constraints (Coin 0) inputBalance' inputIds' of
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
    bundleIsInitiator b =
        isRight $ Selection.create constraints (Coin 0) b [()]
        --c >= mconcat
        --    [ txBaseCost constraints
        --    , txInputCost constraints
        --    , txOutputCost constraints b
        --    , txOutputMinimumAdaQuantity constraints m
        --    ]
    bundleIsSupporter :: TokenBundle -> Bool
    bundleIsSupporter b@(TokenBundle c m) = c >= mconcat
        [ txInputCost constraints
        , txOutputCost constraints b
        , txOutputMinimumAdaQuantity constraints m
        ]

    coinIsIgnorable :: Coin -> Bool
    coinIsIgnorable c = c <= txInputCost constraints

uncategorizeUTxO :: CategorizedUTxO (TxIn, TxOut) -> UTxO
uncategorizeUTxO = UTxO . Map.fromList . fmap fst . uncategorizeUTxOEntries

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
    -> [TokenBundle]
    -- ^ Outputs
    -> TokenBundle
    -- ^ Value to add
    -> NonEmpty TokenBundle
    -- ^ Outputs with the value added
addValueToOutputs constraints outputs = NE.fromList
    . F.foldl' (flip add) outputs
    . splitOutputIfLimitsExceeded constraints
  where
    add :: TokenBundle -> [TokenBundle] -> [TokenBundle]
    add value = run []
      where
        run :: [TokenBundle] -> [TokenBundle] -> [TokenBundle]
        run considered (candidate : unconsidered) =
            case safeMerge value candidate of
                Just merged -> merged : (considered <> unconsidered)
                Nothing -> run (candidate : considered) unconsidered
        run considered [] =
            value : considered

    safeMerge :: TokenBundle -> TokenBundle -> Maybe TokenBundle
    safeMerge a b
        | isSafe = Just value
        | otherwise = Nothing
      where
        isSafe = (&&)
            (txOutputHasValidSize constraints valueWithMaxAda)
            (txOutputHasValidTokenQuantities constraints value)
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

splitOutputIfTokenQuantityExceedsLimit
    :: TxConstraints s
    -> TokenBundle
    -> NonEmpty TokenBundle
splitOutputIfTokenQuantityExceedsLimit
    = flip TokenBundle.equipartitionQuantitiesWithUpperBound
    . txOutputMaximumTokenQuantity

--------------------------------------------------------------------------------
-- Miscellaneous types and functions
--------------------------------------------------------------------------------

maybesToMaybe :: Foldable f => f (Maybe a) -> Maybe a
maybesToMaybe = listToMaybe . catMaybes . F.toList

tails :: NonEmpty a -> NonEmpty (NonEmpty a)
tails (x :| (y : zs)) = (x :| (y : zs)) `NE.cons` tails (y :| zs)
tails (x :| []) = (x :| []) :| []
