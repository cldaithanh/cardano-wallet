{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.CoinSelection.Internal.Types.Value
    where

import Prelude

import Algebra.Difference
    ( Difference )
import Algebra.Equipartition
    ( Equipartition )
import Algebra.Partition
    ( Partition )
import Algebra.Subtract
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
