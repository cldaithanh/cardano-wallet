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
import Algebra.Lattice.Ordered
    ( Ordered (..) )
import Algebra.PartialOrd
    ( PartialOrd )
import Algebra.Partition
    ( Partition )
import Algebra.Subtract
    ( Subtract )
import Data.Monoid
    ( Sum (..) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )

newtype Value = Value {unValue :: Natural}
    deriving (Eq, Generic, Ord)
    deriving PartialOrd via (Ordered Natural)
    deriving (Read, Show) via (Quiet Value)
    deriving newtype (Difference, Equipartition, Partition, Subtract)
    deriving (Monoid, Semigroup) via (Sum Natural)
