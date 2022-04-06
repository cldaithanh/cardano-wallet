{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Wallet.CoinSelection.Asset
    ( WalletAsset (..)
    , tokenBundleAssets
    , tokenBundleAssetCount
    , tokenBundleHasAsset
    , toAssetId
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Control.DeepSeq
    ( NFData )
import Data.Set
    ( Set )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Set as Set

-- | A type capable of representing any asset, including both ada and non-ada
--   assets.
--
-- TODO: ADP-1449
-- Move this type to the wallet-specific 'CoinSelection' module.
--
data WalletAsset
    = WalletAssetLovelace
    | WalletAsset AssetId
    deriving (Eq, Generic, Ord, Read, Show)

deriving instance NFData WalletAsset

-- | Returns the set of assets associated with a given 'TokenBundle'.
--
-- Both ada and non-ada assets are included in the set returned.
--
-- TODO: ADP-1449
-- Move this function to the wallet-specific 'CoinSelection' module.
--
tokenBundleAssets :: TokenBundle -> Set WalletAsset
tokenBundleAssets b = Set.union
    (Set.fromList [WalletAssetLovelace | TokenBundle.coin b /= mempty])
    (Set.map WalletAsset (TokenBundle.getAssets b))

-- | Returns the number of assets associated with a given 'TokenBundle'.
--
-- Both ada and non-ada assets are included in the total count returned.
--
-- TODO: ADP-1449
-- Move this function to the wallet-specific 'CoinSelection' module.
--
tokenBundleAssetCount :: TokenBundle -> Int
tokenBundleAssetCount b = (+)
    (if TokenBundle.coin b /= mempty then 1 else 0)
    (TokenMap.size (TokenBundle.tokens b))

-- | Indicates whether or not a given bundle includes a given asset.
--
-- Both ada and non-ada assets can be queried.
--
-- TODO: ADP-1449
-- Move this function to the wallet-specific 'CoinSelection' module.
--
tokenBundleHasAsset :: TokenBundle -> WalletAsset -> Bool
tokenBundleHasAsset b = \case
    WalletAssetLovelace -> TokenBundle.coin b /= mempty
    WalletAsset assetId -> TokenBundle.hasQuantity b assetId

-- | Extract the asset identifier for an asset, if one is available.
--
-- TODO: ADP-1449
-- Move this function to the wallet-specific 'CoinSelection' module.
--
toAssetId :: WalletAsset -> Maybe AssetId
toAssetId = \case
    WalletAssetLovelace -> Nothing
    WalletAsset assetId -> Just assetId
