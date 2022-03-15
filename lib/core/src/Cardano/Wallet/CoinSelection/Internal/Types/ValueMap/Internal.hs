{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Wallet.CoinSelection.Internal.Types.ValueMap.Internal
    (
--  * Type
      ValueMap

--  * Construction
    , empty

--  * Deconstruction
    , toMap

--  * Queries
    , get

--  * Modification
    , adjust
    )
    where

import Prelude

import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import GHC.Generics
    ( Generic )
import Quiet
    ( Quiet (..) )

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype ValueMap k v = ValueMap
    { unValueMap :: Map k v }
    deriving (Eq, Generic)
    deriving (Read, Show) via (Quiet (ValueMap k v))

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

empty :: ValueMap k v
empty = ValueMap Map.empty

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

toMap :: ValueMap k v -> Map k v
toMap = unValueMap

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

get :: (Ord k, Monoid v) => ValueMap k v -> k -> v
get m k = fromMaybe mempty $ Map.lookup k $ toMap m

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

adjust
    :: (Ord k, Eq v, Monoid v) => ValueMap k v -> k -> (v -> v) -> ValueMap k v
adjust m k f
    | v == mempty = ValueMap $ Map.delete k   $ unValueMap m
    | otherwise   = ValueMap $ Map.insert k v $ unValueMap m
  where
    v = f $ get m k
