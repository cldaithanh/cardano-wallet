{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- An address pool caches a collection of addresses.
-- The purpose of this data structure is to aid in BIP-44 style
-- address discovery with an address gap.
module Cardano.Wallet.Address.Pool'
    (
    -- * Abstract data type
      Pool

    -- * Helper types
    , PoolState(..)
    , PoolGap

    -- * Constructors
    , new
    , loadUnsafe

    -- * Operations
    , allocate
    , clear

    -- * Observations
    , gap
    , usage
    , lookup
    , size
    , next

    -- ** Denotation
    , info
    )
  where

import Prelude hiding
    ( last, lookup )

import Control.DeepSeq
    ( NFData )
import Data.Map.Strict
    ( Map )
import GHC.Generics
    ( Generic )

import qualified Data.Map.Strict as Map

type PoolGap = Int

data PoolState = Used | Unused
    deriving (Eq, Show, Generic)

instance NFData PoolState

data Pool ix = Pool
    { gap :: PoolGap
    , usage :: Map ix PoolState
    } deriving (Generic)

instance NFData ix => NFData (Pool ix)

instance Eq ix => Eq (Pool ix) where
    a == b = info a == info b

instance Show ix => Show (Pool ix) where
    show = show . info

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

-- | Create a new address pool.
new :: (Ord ix, Enum ix) => PoolGap -> Pool ix
new poolGap =
    Pool poolGap $ Map.fromList [(i, Unused) | i <- [toEnum 0..(toEnum $ poolGap-1)]]

-- | Replace the pool state but skips checking the invariants.
loadUnsafe :: Pool ix -> Map ix PoolState -> Pool ix
loadUnsafe pool state = pool { usage = state }

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Remove all previously discovered addresses,
-- i.e. create a new pool with the same 'generator' and 'gap' as the old pool.
clear :: (Ord ix, Enum ix) => Pool ix -> Pool ix
clear = new . gap

-- | Sets the index to the 'Used' status, and ensures pool gap is maintained.
allocate :: (Ord ix, Enum ix) => ix -> Pool ix -> Pool ix
allocate ix pool@(Pool { usage }) =
    case Map.lookup ix usage of
        Nothing -> pool
        Just _  -> ensureFresh (succ ix) $
            pool { usage = Map.adjust (const Used) ix usage }

-- Observations
-- | Denotation of a Pool.
info :: Pool ix -> (PoolGap, Map ix PoolState)
info (Pool gap usage) = (gap, usage)

-- | Number of indices cached in the pool.
size :: Pool ix -> Int
size = Map.size . usage

-- | Look up a usage state in the pool.
lookup :: Ord ix => ix -> Pool ix -> Maybe PoolState
lookup ix = Map.lookup ix . usage

-- | Given an index, retrieve the next index that is still in the pool.
next :: Enum ix => Pool ix -> ix -> Maybe ix
next Pool{usage} ix = let jx = succ ix in
    if fromEnum jx >= Map.size usage then Nothing else Just jx

-- | Create additional 'Unused' addresses from larger indices
-- in order to satisfy 'prop_fresh' again.
--
-- Precondition: Either @ix = fromEnum 0@,
-- or the index @jx@ which satisfies to @ix = succ jx@
-- is 'Used'.
ensureFresh :: (Ord ix, Enum ix) => ix -> Pool ix -> Pool ix
ensureFresh ix pool@(Pool gap usage)
    = pool { usage = Map.union usage nexts }
  where
    fresh = toEnum $ Map.size usage -- first index that is not in the pool
    nexts = Map.fromList
        [ ((i, Unused)) | i <- [fresh .. to] ]
      where
        to = toEnum $ fromEnum ix + fromIntegral gap - 1
