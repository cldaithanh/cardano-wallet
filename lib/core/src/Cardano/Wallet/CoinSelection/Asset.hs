{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Wallet.CoinSelection.Asset
    ( Asset (..)
    , tokenBundleAssets
    , tokenBundleAssetCount
    , tokenBundleHasAsset
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
data Asset
    = AssetLovelace
    | Asset AssetId
    deriving (Eq, Generic, Ord, Read, Show)

deriving instance NFData Asset

-- | Returns the set of assets associated with a given 'TokenBundle'.
--
-- Both ada and non-ada assets are included in the set returned.
--
-- TODO: ADP-1449
-- Move this function to the wallet-specific 'CoinSelection' module.
--
tokenBundleAssets :: TokenBundle -> Set Asset
tokenBundleAssets b = Set.union
    (Set.fromList [AssetLovelace | TokenBundle.coin b /= mempty])
    (Set.map Asset (TokenBundle.getAssets b))

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
tokenBundleHasAsset :: TokenBundle -> Asset -> Bool
tokenBundleHasAsset b = \case
    AssetLovelace -> TokenBundle.coin b /= mempty
    Asset assetId -> TokenBundle.hasQuantity b assetId
