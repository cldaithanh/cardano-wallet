{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.CoinSelection.Internal.Types
    where

import Prelude hiding
    ( subtract )

import Cardano.Wallet.CoinSelection.Internal.Types.AssetValueMap
    ( AssetValueMap )
import Cardano.Wallet.CoinSelection.Internal.Types.Value
    ( Value (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection )
import Data.Bifunctor
    ( bimap )
import Data.Functor
    ( (<&>) )
import Data.List
    ( sortOn )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import GHC.Exts
    ( IsList (..) )

import qualified Cardano.Wallet.CoinSelection.Internal.Types.AssetValueMap as AssetValueMap
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
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
    type Asset Wallet = WalletAsset
    type UTxO Wallet = WalletUTxO

data WalletAsset
    = WalletAssetLovelace
    | WalletAsset W.AssetId
    deriving (Eq, Ord, Show)

data WalletUTxO = WalletUTxO
    { txIn
        :: W.TxIn
    , address
        :: W.Address
    }
    deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

walletAssetToAssetId :: WalletAsset -> Maybe W.AssetId
walletAssetToAssetId = \case
    WalletAssetLovelace -> Nothing
    WalletAsset assetId -> Just assetId

tokenBundleToAssetValueMap :: TokenBundle -> AssetValueMap WalletAsset
tokenBundleToAssetValueMap (TokenBundle (Coin c) m) = fromList $ (:)
    (WalletAssetLovelace, Value c)
    (bimap WalletAsset (Value . unTokenQuantity) <$> TokenMap.toFlatList m)

assetValueMapToTokenBundle :: AssetValueMap WalletAsset -> TokenBundle
assetValueMapToTokenBundle vm = TokenBundle c m
  where
    c = Coin $ unValue $ vm `AssetValueMap.get` WalletAssetLovelace
    m = TokenMap.fromFlatList $ fmap (TokenQuantity . unValue) <$> mapMaybe
        (\(k, v) -> walletAssetToAssetId k <&> (, v))
        (toList vm)
