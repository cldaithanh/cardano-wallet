{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.CoinSelection.Internal.Types.ValueMap
    ( ValueMap
    , Keys (..)
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

newtype ValueMap a = ValueMap
    {unValueMap :: MonoidMap a Value}
    deriving (Eq, Generic, IsList, Monoid, Semigroup)
    deriving (Difference, PartialOrd, Partition, Subtract)
    deriving (Read, Show) via (Quiet (MonoidMap a Value))

newtype Keys a = Keys {unKeys :: ValueMap a}
    deriving (Eq, Monoid, Semigroup)
    deriving Equipartition via (MonoidMap.Keys (MonoidMap a Value))

newtype Values a = Values {unValues :: ValueMap a}
    deriving (Eq, Monoid, Semigroup)
    deriving Equipartition via (MonoidMap.Values (MonoidMap a Value))

get :: Ord a => ValueMap a -> a -> Value
get = MonoidMap.get . unValueMap
