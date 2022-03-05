{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.CoinSelection.Internal.Types.Value
    where

import Prelude

import Cardano.Wallet.CoinSelection.Internal.Types.Difference
    ( Difference )
import Cardano.Wallet.CoinSelection.Internal.Types.Equipartition
    ( Equipartition )
import Cardano.Wallet.CoinSelection.Internal.Types.Partition
    ( Partition )
import Cardano.Wallet.CoinSelection.Internal.Types.Subtract
    ( Subtract )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )

newtype Value = Value {unValue :: Natural}
    deriving (Eq, Generic, Ord)
    deriving (Read, Show) via (Quiet Value)
    deriving newtype (Difference, Equipartition, Partition, Subtract)

instance Monoid Value where
    mempty = Value 0

instance Semigroup Value where
    Value v1 <> Value v2 = Value (v1 + v2)
