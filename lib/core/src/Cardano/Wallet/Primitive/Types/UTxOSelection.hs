{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Primitive.Types.UTxOSelection
    (
      -- * Classes
      IsUTxOSelection

      -- * Types
    , UTxOSelection
    , UTxOSelectionNonEmpty

      -- * Construction
    , fromIndex

      -- * Promotion
    , toNonEmpty

      -- * Querying
    , leftoverCount
    , leftoverIndex
    , leftoverList
    , selectedCount
    , selectedIndex
    , selectedList

      -- * Modification
    , select

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( over )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.List.NonEmpty as NonEmpty

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class HasUTxOSelectionState s where

    -- | Retrieves the internal state.
    state :: s -> State

class HasUTxOSelectionState s => IsUTxOSelection s where

    -- | The type of the list of selected UTxOs.
    type SelectedList s

    -- | Retrieves a list of the selected UTxOs.
    selectedList :: s -> SelectedList s

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data State = State
    { selected :: !UTxOIndex
    , leftover :: !UTxOIndex
    }
    deriving (Eq, Generic, Show)

-- | A selection for which 'selectionCount' may be zero.
--
newtype UTxOSelection = UTxOSelection State
    deriving (Eq, Generic, Show)

-- | A selection for which 'selectionCount' cannot be zero.
--
newtype UTxOSelectionNonEmpty = UTxOSelectionNonEmpty State
    deriving (Eq, Generic, Show)

instance HasUTxOSelectionState UTxOSelection where
    state (UTxOSelection s) = s

instance HasUTxOSelectionState UTxOSelectionNonEmpty where
    state (UTxOSelectionNonEmpty s) = s

instance IsUTxOSelection UTxOSelection where
    type SelectedList UTxOSelection = [(TxIn, TxOut)]
    selectedList
        = UTxOIndex.toList
        . selectedIndex

instance IsUTxOSelection UTxOSelectionNonEmpty where
    type SelectedList UTxOSelectionNonEmpty = NonEmpty (TxIn, TxOut)
    selectedList
        = NonEmpty.fromList
        . UTxOIndex.toList
        . selectedIndex

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Creates a selection from an index and a selection filter.
--
fromIndex :: UTxOIndex -> (TxIn -> Bool) -> UTxOSelection
fromIndex index select =
    UTxOSelection State {selected, leftover}
  where
    (selected, leftover) = UTxOIndex.partition select index

--------------------------------------------------------------------------------
-- Promotion
--------------------------------------------------------------------------------

toNonEmpty :: UTxOSelection -> Maybe UTxOSelectionNonEmpty
toNonEmpty s
    | selectedCount s == 0 =
        Nothing
    | otherwise =
        Just $ UTxOSelectionNonEmpty $ state s

--------------------------------------------------------------------------------
-- Querying
--------------------------------------------------------------------------------

-- | Retrieves a count of the leftover UTxOs.
--
leftoverCount :: IsUTxOSelection s => s -> Int
leftoverCount = UTxOIndex.size . leftoverIndex

-- | Retrieves an index of the leftover UTxOs.
--
leftoverIndex :: IsUTxOSelection s => s -> UTxOIndex
leftoverIndex = leftover . state

-- | Retrieves a list of the leftover UTxOs.
--
leftoverList :: IsUTxOSelection s => s -> [(TxIn, TxOut)]
leftoverList = UTxOIndex.toList . leftoverIndex

-- | Retrieves a count of the selected UTxOs.
--
selectedCount :: IsUTxOSelection s => s -> Int
selectedCount = UTxOIndex.size . selectedIndex

-- | Retrieves an index of the selected UTxOs.
--
selectedIndex :: IsUTxOSelection s => s -> UTxOIndex
selectedIndex = selected . state

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

-- | Moves an entry from the leftover set to the selected set.
--
select :: IsUTxOSelection s => TxIn -> s -> Maybe (UTxOSelectionNonEmpty)
select i = fmap UTxOSelectionNonEmpty . updateState . state
  where
    updateState :: State -> Maybe State
    updateState s =
        updateFields <$> UTxOIndex.lookup i (leftover s)
      where
        updateFields o = s
            & over #selected (UTxOIndex.insert i o)
            & over #leftover (UTxOIndex.delete i)
