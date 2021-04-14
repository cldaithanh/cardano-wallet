{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Migration
    (
      -- * UTxO entry classification
      classifyUTxO
    , classifyUTxOEntries
    , ClassifiedUTxO (..)

      -- * Migration plans
    , createPlan
    , MigrationPlan (..)

      -- * Utility functions
    , addValueToOutputs

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
    run [] (classifyUTxOEntries constraints entries)
  where
    run selections utxo rewardBalance =
        case createSelection constraints utxo rewardBalance of
            Just (utxo', selection) ->
                run (selection : selections) utxo' (Coin 0)
            Nothing -> MigrationPlan
                { selections
                , unselected = unclassifyUTxOEntries utxo
                , totalFee = F.foldMap (view #fee) selections
                }

createSelection
    :: TxSize s
    => TxConstraints s
    -> ClassifiedUTxO i
    -> Coin
    -- ^ Reward balance
    -> Maybe (ClassifiedUTxO i, Selection i s)
createSelection constraints utxo rewardBalance =
    extendSelection constraints
        =<< initializeSelection constraints utxo rewardBalance

initializeSelection
    :: TxSize s
    => TxConstraints s
    -> ClassifiedUTxO i
    -> Coin
    -- ^ Reward balance
    -> Maybe (ClassifiedUTxO i, Selection i s)
initializeSelection constraints utxoAtStart rewardBalance
    | Just (supporter, utxo) <- selectSupporter utxoAtStart =
        run utxo (supporter :| [])
    | otherwise =
        Nothing
  where
    run utxo inputs =
        case Selection.create constraints rewardBalance inputs of
            Right selection ->
                Just (utxo, selection)
            Left SelectionAdaInsufficient ->
                case selectSupporter utxo of
                    Just (input, utxo') ->
                        run utxo' (input `NE.cons` inputs)
                    Nothing ->
                        Nothing
            Left (SelectionFull _) ->
                Nothing

extendSelection
    :: TxSize s
    => TxConstraints s
    -> (ClassifiedUTxO i, Selection i s)
    -> Maybe (ClassifiedUTxO i, Selection i s)
extendSelection constraints = extendSelectionWithFreerider
  where
    extendSelectionWithFreerider (utxo, selection) =
        case extendSelectionWithEntry constraints Freerider (utxo, selection) of
            Right (utxo', selection') ->
                extendSelectionWithFreerider (utxo', selection')
            Left ExtendSelectionAdaInsufficient ->
                extendSelectionWithSupporter (utxo, selection)
            Left ExtendSelectionEntriesExhausted ->
                extendSelectionWithSupporter (utxo, selection)
            Left ExtendSelectionFull ->
                Just (utxo, selection)

    extendSelectionWithSupporter (utxo, selection) =
        case extendSelectionWithEntry constraints Supporter (utxo, selection) of
            Right (utxo', selection') ->
                extendSelectionWithFreerider (utxo', selection')
            Left ExtendSelectionAdaInsufficient ->
                Just (utxo, selection)
            Left ExtendSelectionEntriesExhausted ->
                Just (utxo, selection)
            Left ExtendSelectionFull ->
                Just (utxo, selection)

data ExtendSelectionError
    = ExtendSelectionAdaInsufficient
    | ExtendSelectionEntriesExhausted
    | ExtendSelectionFull

extendSelectionWithEntry
    :: TxSize s
    => TxConstraints s
    -> UTxOEntryClassification
    -> (ClassifiedUTxO i, Selection i s)
    -> Either ExtendSelectionError (ClassifiedUTxO i, Selection i s)
extendSelectionWithEntry constraints classification (utxo, selection) =
    case selectWithClassification classification utxo of
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

selectWithClassification
    :: UTxOEntryClassification
    -> ClassifiedUTxO i
    -> Maybe ((i, TokenBundle), ClassifiedUTxO i)
selectWithClassification = \case
    Supporter -> selectSupporter
    Freerider -> selectFreerider
    Ignorable -> const Nothing

selectSupporter
    :: ClassifiedUTxO i
    -> Maybe ((i, TokenBundle), ClassifiedUTxO i)
selectSupporter utxo = case supporters utxo of
    supporter : remaining ->
        Just (supporter, utxo {supporters = remaining})
    [] ->
        Nothing

selectFreerider
    :: ClassifiedUTxO i
    -> Maybe ((i, TokenBundle), ClassifiedUTxO i)
selectFreerider utxo = case freeriders utxo of
    freerider : remaining ->
        Just (freerider, utxo {freeriders = remaining})
    [] ->
        Nothing

--------------------------------------------------------------------------------
-- Classification of UTxO entries
--------------------------------------------------------------------------------

data ClassifiedUTxO i = ClassifiedUTxO
    { supporters :: [(i, TokenBundle)]
    , freeriders :: [(i, TokenBundle)]
    , ignorables :: [(i, TokenBundle)]
    }
    deriving (Eq, Show)

classifyUTxO
    :: TxSize s
    => TxConstraints s
    -> UTxO
    -> ClassifiedUTxO (TxIn, TxOut)
classifyUTxO constraints (UTxO u) =
    classifyUTxOEntries constraints
        ((\(i, o) -> ((i, o), view #tokens o)) <$> Map.toList u)

classifyUTxOEntries
    :: forall i s. TxSize s
    => TxConstraints s
    -> [(i, TokenBundle)]
    -> ClassifiedUTxO i
classifyUTxOEntries constraints unclassifiedEntries = ClassifiedUTxO
    { supporters = entriesMatching Supporter
    , freeriders = entriesMatching Freerider
    , ignorables = entriesMatching Ignorable
    }
  where
    entries :: [(i, (TokenBundle, UTxOEntryClassification))]
    entries = unclassifiedEntries
        <&> (\(i, b) -> (i, (b, classifyUTxOEntry constraints b)))

    entriesMatching :: UTxOEntryClassification -> [(i, TokenBundle)]
    entriesMatching classification =
        fmap fst <$> L.filter ((== classification) . snd . snd) entries

data UTxOEntryClassification
    = Supporter
    -- ^ A coin or token bundle that is capable of paying for its own marginal
    -- fee.
    | Freerider
    -- ^ A coin or token bundle that is not capable of paying for its own
    -- marginal fee.
    | Ignorable
    -- ^ A coin that should not be added to a selection, because its value is
    -- lower than the marginal fee for an input.
    deriving (Eq, Show)

classifyUTxOEntry
    :: TxSize s
    => TxConstraints s
    -> TokenBundle
    -> UTxOEntryClassification
classifyUTxOEntry constraints b
    | Just c <- TokenBundle.toCoin b, coinIsIgnorable c =
        Ignorable
    | Just _ <- TokenBundle.toCoin b =
        Supporter
    | bundleIsSupporter b =
        Supporter
    | otherwise =
        Freerider
  where
    bundleIsSupporter :: TokenBundle -> Bool
    bundleIsSupporter b@(TokenBundle c m) =
        case computeOutputCoin of
            Nothing -> False
            Just oc -> txOutputIsValid constraints (TokenBundle oc m)
      where
        computeOutputCoin :: Maybe Coin
        computeOutputCoin = coinFromInteger
            $ coinToInteger c
            - coinToInteger (txInputCost constraints)
            - coinToInteger (txOutputCost constraints b)

    coinIsIgnorable :: Coin -> Bool
    coinIsIgnorable c = c <= txInputCost constraints

unclassifyUTxOEntries :: ClassifiedUTxO i -> [(i, TokenBundle)]
unclassifyUTxOEntries utxo = mconcat
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

coinFromInteger :: Integer -> Maybe Coin
coinFromInteger i
    | i < fromIntegral (unCoin $ minBound @Coin) = Nothing
    | i > fromIntegral (unCoin $ maxBound @Coin) = Nothing
    | otherwise = Just $ Coin $ fromIntegral i

coinToInteger :: Coin -> Integer
coinToInteger = fromIntegral . unCoin
