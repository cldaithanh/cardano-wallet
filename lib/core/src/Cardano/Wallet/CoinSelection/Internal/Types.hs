{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.CoinSelection.Internal.Types
    where

import Prelude hiding
    ( subtract )

import Cardano.Wallet.CoinSelection.Internal.Types.Value
    ( Value )
import Cardano.Wallet.CoinSelection.Internal.Types.ValueMap
    ( ValueMap )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection )
import Data.List
    ( sortOn )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )

import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

class
    ( Ord (Address c)
    , Ord (Asset c)
    , Ord (UTxO c)
    ) =>
    SelectionContext c
  where
    type Address c
    type Asset c
    type UTxO c

type AssetValueMap c = ValueMap (Asset c) Value

data SelectionConstraints c = SelectionConstraints
    { computeMinimumCost
        :: Selection c -> AssetValueMap c
    , exceedsMaximumAssetValueMapSize
        :: AssetValueMap c -> Bool
    , exceedsMaximumAssetValue
        :: AssetValueMap c -> Bool
    , provideMinimumAssetValues
        :: AssetValueMap c -> AssetValueMap c
    }

data SelectionParams c = SelectionParams
    { outputs
        :: [(Address c, AssetValueMap c)]
    , utxoAvailableForCollateral
        :: Map (UTxO c) Value
    , utxoAvailableForInputs
        :: UTxOSelection (UTxO c)
    }

data Selection c = Selection
    { inputs
        :: NonEmpty (UTxO c, AssetValueMap c)
    , collateral
        :: [(UTxO c, Value)]
    , outputs
        :: [(Address c, AssetValueMap c)]
    }

performSelection
    :: SelectionContext c
    => SelectionParams c
    -> Selection c
performSelection SelectionParams {outputs} = Selection
    { inputs = undefined
    , collateral = []
    , outputs = sortOn fst outputs
    }

--------------------------------------------------------------------------------
-- Example context
--------------------------------------------------------------------------------

data Wallet

instance SelectionContext Wallet where
    type Address Wallet = W.Address
    type Asset Wallet = W.AssetId
    type UTxO Wallet = (W.TxIn, W.Address)
