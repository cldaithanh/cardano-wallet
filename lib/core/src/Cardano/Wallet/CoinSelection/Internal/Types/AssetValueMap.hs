{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.CoinSelection.Internal.Types.AssetValueMap
    ( AssetValueMap
    , Assets (..)
    , Values (..)
    , get
    )
    where

import Prelude

import Algebra.Difference
    ( Difference )
import Algebra.Equipartition
    ( Equipartition (..) )
import Algebra.PartialOrd
    ( PartialOrd )
import Algebra.Partition
    ( Partition )
import Algebra.Subtract
    ( Subtract )
import Cardano.Wallet.CoinSelection.Internal.Types.Value
    ( Value )
import Data.MonoidMap
    ( MonoidMap )
import GHC.Exts
    ( IsList )
import GHC.Generics
    ( Generic )
import Quiet
    ( Quiet (..) )

import qualified Data.MonoidMap as MonoidMap

newtype AssetValueMap a = AssetValueMap
    {unAssetValueMap :: MonoidMap a Value}
    deriving (Eq, Generic, IsList, Monoid, Semigroup)
    deriving (Difference, PartialOrd, Partition, Subtract)
    deriving (Read, Show) via (Quiet (MonoidMap a Value))

newtype Assets a = Assets {unAssets :: AssetValueMap a}
    deriving (Eq, Monoid, Semigroup)
    deriving Equipartition via (MonoidMap.Keys (MonoidMap a Value))

newtype Values a = Values {unValues :: AssetValueMap a}
    deriving (Eq, Monoid, Semigroup)
    deriving Equipartition via (MonoidMap.Values (MonoidMap a Value))

get :: Ord a => AssetValueMap a -> a -> Value
get = MonoidMap.get . unAssetValueMap
