{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Migration
    where

import Prelude

import Cardano.Wallet.Primitive.Migration.SelectionParameters
    ( SelectionParameters (..), tokenBundleIsValid )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Data.Either.Extra
    ( eitherToMaybe )
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

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

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

guardE :: Bool -> e -> Either e ()
guardE = undefined

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
