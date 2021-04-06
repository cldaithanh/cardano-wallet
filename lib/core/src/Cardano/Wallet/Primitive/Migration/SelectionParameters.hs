{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Migration.SelectionParameters
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
    ( TokenBundleSizeAssessment (..), TokenBundleSizeAssessor (..) )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Maybe
    ( fromMaybe )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap

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

-- | The amount of ada an output has in excess of its minimum ada quantity.
--
excessAdaForOutput :: SelectionParameters s -> TokenBundle -> Coin
excessAdaForOutput params bundle =
    fromMaybe (Coin 0) result
  where
    result = subtractCoin
        (view #coin bundle)
        (minimumAdaQuantityForOutput params $ view #tokens bundle)

-- | The variable fee for an output coin.
--
feeForOutputCoin :: SelectionParameters s -> Coin -> Coin
feeForOutputCoin params = feeForOutput params . TokenBundle.fromCoin

-- | The constant minimum ada quantity for a pure ada output.
--
minimumAdaQuantityForOutputCoin :: SelectionParameters s -> Coin
minimumAdaQuantityForOutputCoin =
    flip minimumAdaQuantityForOutput TokenMap.empty

sizeOfOutputCoin :: SelectionParameters s -> Coin -> s
sizeOfOutputCoin = undefined

-- | Indicates whether or not the given token bundle is valid for inclusion in
--   a selection output.
--
tokenBundleIsValid :: SelectionParameters s -> TokenBundle -> Bool
tokenBundleIsValid params b = and $ conditions <&> (\f -> f params b)
  where
    conditions :: [SelectionParameters s -> TokenBundle -> Bool]
    conditions =
        [ tokenBundleSatisfiesMinimumAdaQuantity
        , tokenBundleSizeWithinLimit
        , tokenBundleQuantitiesWithinLimit
        ]

-- | Indicates whether or not all the quantities within the given bundle are
--   within the limit of what can be included in a selection output.
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
--   limit of what can be included in a selection output.
--
tokenBundleSizeWithinLimit :: SelectionParameters s -> TokenBundle -> Bool
tokenBundleSizeWithinLimit params b = case assess b of
    TokenBundleSizeWithinLimit -> True
    OutputTokenBundleSizeExceedsLimit -> False
  where
    TokenBundleSizeAssessor assess = tokenBundleSizeAssessor params

-- | Indicates whether or not the given token quantity is within the limit of
--   what can be included in a selection output.
--
tokenQuantityWithinLimit :: SelectionParameters s -> TokenQuantity -> Bool
tokenQuantityWithinLimit params q = case assess q of
    TokenQuantityWithinLimit -> True
    TokenQuantityExceedsLimit -> False
  where
    TokenQuantityAssessor assess = tokenQuantityAssessor params

newtype TokenQuantityAssessor = TokenQuantityAssessor
    { assessTokenQuantity :: TokenQuantity -> TokenQuantityAssessment
    }

data TokenQuantityAssessment
    = TokenQuantityWithinLimit
    | TokenQuantityExceedsLimit
    deriving (Eq, Generic, Show)
