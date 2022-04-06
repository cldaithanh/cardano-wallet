{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{- HLINT ignore "Use &&" -}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Provides internal functions for the 'UTxOIndex' type, which indexes a UTxO
-- set by asset identifier.
--
-- The index makes it possible to efficiently compute the subset of a UTxO set
-- containing a particular asset, or to select just a single UTxO containing a
-- particular asset, without having to search linearly through the entire UTxO
-- set.
--
-- See the documentation for 'UTxOIndex' for more details.
--
module Cardano.Wallet.Primitive.Types.UTxOIndex.Internal
    (
    ----------------------------------------------------------------------------
    -- Public Interface
    ----------------------------------------------------------------------------

    -- * Type

      -- Important:
      --
      -- The default data constructor for 'UTxOIndex' is not exported, by
      -- design, as the internal data structure has an invariant that must
      -- be preserved across all operations.
      --
      -- See the 'checkInvariant' function for more details.
      --
      UTxOIndex

    -- * Construction
    , empty
    , singleton
    , fromSequence
    , fromMap

    -- * Deconstruction
    , toList
    , toMap

    -- * Folding
    , fold

    -- * Modification
    , insert
    , insertMany
    , delete
    , deleteMany

    -- * Filtering and partitioning
    , filter
    , partition

    -- * Queries
    , assets
    , balance
    , lookup
    , member
    , null
    , size

    -- * Set operations
    , difference
    , disjoint

    -- * Selection
    , SelectionFilter (..)
    , selectRandom
    , selectRandomWithPriority

    ----------------------------------------------------------------------------
    -- Internal Interface
    ----------------------------------------------------------------------------

    -- * Token bundle categorization
    , BundleCategory (..)
    , categorizeTokenBundle

    -- * Utilities
    , selectRandomSetMember

    -- * Invariant
    , InvariantStatus (..)
    , checkInvariant

    ) where

import Prelude hiding
    ( filter, lookup, null )

import Cardano.Wallet.CoinSelection.Internal.Context
    ( SelectionContext (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Control.Monad.Extra
    ( firstJustM )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( over )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust )
import Data.Set
    ( Set )
import Data.Set.Strict.NonEmptySet
    ( NonEmptySet )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set.Strict.NonEmptySet as NonEmptySet

--------------------------------------------------------------------------------
-- Public Interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | A UTxO set that is indexed by asset identifier.
--
-- The index provides a mapping from assets to subsets of the UTxO set.
--
-- A UTxO appears in the set for a particular asset if and only if its
-- associated value has a non-zero quantity of that asset.
--
-- The index makes it possible to efficiently compute the subset of a UTxO set
-- containing a particular asset, or to select just a single UTxO containing a
-- particular asset, without having to search linearly through the entire UTxO
-- set.
--
-- The index also keeps track of the current UTxO balance of all assets, making
-- it possible to efficiently look up the total quantity of a particular asset
-- without having to sum across the entire UTxO set.
--
-- The UTxO index data structure has an invariant that can be checked with
-- the 'checkInvariant' function.
--
data UTxOIndex ctx = UTxOIndex
    { indexAll
        :: !(Map (Asset ctx) (NonEmptySet (UTxO ctx)))
        -- An index of all entries that contain the given asset.
    , indexSingletons
        :: !(Map (Asset ctx) (NonEmptySet (UTxO ctx)))
        -- An index of all entries that contain the given asset and no other
        -- assets.
    , indexPairs
        :: !(Map (Asset ctx) (NonEmptySet (UTxO ctx)))
        -- An index of all entries that contain the given asset and exactly
        -- one other asset.
    , balance
        :: !TokenBundle
        -- The total balance of all entries.
    , universe
        :: !(Map (UTxO ctx) TokenBundle)
        -- The complete set of all entries.
    }

deriving instance SelectionContext ctx => Eq (UTxOIndex ctx)
deriving instance Generic (UTxOIndex ctx)
deriving instance SelectionContext ctx => Show (UTxOIndex ctx)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | An index with no entries.
--
empty :: UTxOIndex ctx
empty = UTxOIndex
    { indexAll = Map.empty
    , indexSingletons = Map.empty
    , indexPairs = Map.empty
    , balance = TokenBundle.empty
    , universe = Map.empty
    }

-- | Creates a singleton index from the specified UTxO identifier and value.
--
singleton :: SelectionContext ctx => UTxO ctx -> TokenBundle -> UTxOIndex ctx
singleton u b = insertUnsafe u b empty

-- | Constructs an index from a sequence of entries.
--
-- Note that this operation is potentially expensive as it must construct an
-- index from scratch, and therefore should only be used sparingly.
--
-- If the given sequence contains more than one mapping for the same UTxO
-- identifier, the mapping that appears latest in the sequence will take
-- precedence, and all others will be ignored.
--
fromSequence
    :: (Foldable f, SelectionContext ctx)
    => f (UTxO ctx, TokenBundle)
    -> UTxOIndex ctx
fromSequence = flip insertMany empty

-- | Constructs an index from a map.
--
-- Note that this operation is potentially expensive as it must construct an
-- index from scratch, and therefore should only be used sparingly.
--
fromMap :: SelectionContext ctx => Map (UTxO ctx) TokenBundle -> UTxOIndex ctx
fromMap = fromSequence . Map.toList

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

-- | Converts an index to a list of its constituent entries.
--
-- Consider using 'fold' if your goal is to consume all entries in the output.
--
toList :: UTxOIndex ctx -> [(UTxO ctx, TokenBundle)]
toList = fold (\ubs u b -> (u, b) : ubs) []

-- | Converts an index into a map.
--
-- Consider using 'fold' if your goal is to consume all entries in the output.
--
toMap :: UTxOIndex ctx -> Map (UTxO ctx) TokenBundle
toMap = universe

--------------------------------------------------------------------------------
-- Folding
--------------------------------------------------------------------------------

-- | Folds strictly over the constituent entries of an index.
--
fold :: (a -> UTxO ctx -> TokenBundle -> a) -> a -> UTxOIndex ctx -> a
fold f a = Map.foldlWithKey' f a . universe

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

-- | Inserts an entry that maps the given UTxO identifier to the given value.
--
-- If the index has an existing value for the specified UTxO identifier, the
-- value referred to by that identifier will be replaced with the specified
-- value.
--
insert
    :: SelectionContext ctx
    => UTxO ctx
    -> TokenBundle
    -> UTxOIndex ctx
    -> UTxOIndex ctx
insert u b = insertUnsafe u b . delete u

-- | Inserts multiple entries into an index.
--
-- See 'insert'.
--
insertMany
    :: Foldable f
    => SelectionContext ctx
    => f (UTxO ctx, TokenBundle)
    -> UTxOIndex ctx
    -> UTxOIndex ctx
insertMany = flip $ F.foldl' $ \i (u, b) -> insert u b i

-- | Deletes the entry corresponding to the given UTxO identifier.
--
-- If the index has no existing entry for the specified identifier, the result
-- of applying this function will be equivalent to the identity function.
--
delete
    :: forall ctx. SelectionContext ctx
    => UTxO ctx
    -> UTxOIndex ctx
    -> UTxOIndex ctx
delete u i =
    maybe i updateIndex $ Map.lookup u $ universe i
  where
    updateIndex :: TokenBundle -> UTxOIndex ctx
    updateIndex b = i
        -- This operation is safe, since we have already determined that the
        -- entry is a member of the index, and therefore the balance must be
        -- greater than or equal to the value of this output:
        & over #balance (`TokenBundle.unsafeSubtract` b)
        & over #universe (Map.delete u)
        & case categorizeTokenBundle b of
            BundleWithNoAssets -> id
            BundleWithOneAsset a -> id
                . over #indexAll (`deleteEntry` a)
                . over #indexSingletons (`deleteEntry` a)
            BundleWithTwoAssets (a1, a2) -> id
                . over #indexAll (`deleteEntry` a1)
                . over #indexAll (`deleteEntry` a2)
                . over #indexPairs (`deleteEntry` a1)
                . over #indexPairs (`deleteEntry` a2)
            BundleWithMultipleAssets as -> id
                . over #indexAll (flip (F.foldl' deleteEntry) as)

    deleteEntry
        :: Map (Asset ctx) (NonEmptySet (UTxO ctx))
        -> Asset ctx
        -> Map (Asset ctx) (NonEmptySet (UTxO ctx))
    deleteEntry m a = Map.update (NonEmptySet.delete u) a m

-- | Deletes multiple entries from an index.
--
-- See 'delete'.
--
deleteMany
    :: (Foldable f, SelectionContext ctx)
    => f (UTxO ctx)
    -> UTxOIndex ctx
    -> UTxOIndex ctx
deleteMany = flip $ F.foldl' $ \i u -> delete u i

--------------------------------------------------------------------------------
-- Filtering and partitioning
--------------------------------------------------------------------------------

-- | Filters an index.
--
filter
    :: SelectionContext ctx
    => (UTxO ctx -> Bool)
    -> UTxOIndex ctx
    -> UTxOIndex ctx
filter f = fromSequence . L.filter (f . fst) . toList

-- | Partitions an index.
--
partition
    :: SelectionContext ctx
    => (UTxO ctx -> Bool)
    -> UTxOIndex ctx
    -> (UTxOIndex ctx, UTxOIndex ctx)
partition f = bimap fromSequence fromSequence . L.partition (f . fst) . toList

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- | Returns the complete set of all assets contained in an index.
--
assets :: UTxOIndex ctx -> Set (Asset ctx)
assets = Map.keysSet . indexAll

-- | Returns the value corresponding to the given UTxO identifier.
--
-- If the index has no such identifier, this function returns 'Nothing'.
--
lookup :: SelectionContext ctx => UTxO ctx -> UTxOIndex ctx -> Maybe TokenBundle
lookup u = Map.lookup u . universe

-- | Returns 'True' if (and only if) the index has an entry for the given UTxO
--   identifier.
--
member :: SelectionContext ctx => UTxO ctx -> UTxOIndex ctx -> Bool
member u = isJust . lookup u

-- | Returns 'True' if (and only if) the index is empty.
--
null :: UTxOIndex ctx -> Bool
null = (== 0) . size

-- | Returns the total number of UTxO entries held within the index.
--
size :: UTxOIndex ctx -> Int
size = Map.size . universe

--------------------------------------------------------------------------------
-- Set operations
--------------------------------------------------------------------------------

difference
    :: SelectionContext ctx
    => UTxOIndex ctx
    -> UTxOIndex ctx
    -> UTxOIndex ctx
difference a b = fromSequence
    $ Map.toList
    $ Map.difference (universe a) (universe b)

-- | Indicates whether a pair of UTxO indices are disjoint.
--
disjoint :: SelectionContext ctx => UTxOIndex ctx -> UTxOIndex ctx -> Bool
disjoint i1 i2 = universe i1 `Map.disjoint` universe i2

--------------------------------------------------------------------------------
-- Selection
--------------------------------------------------------------------------------

-- | Specifies a filter for selecting UTxO entries.
--
data SelectionFilter asset
    = SelectSingleton asset
      -- ^ Matches UTxOs that contain only the given asset and no other assets.
    | SelectPairWith asset
      -- ^ Matches UTxOs that contain the given asset and exactly one other
      -- asset.
    | SelectAnyWith asset
      -- ^ Matches UTxOs that contain the given asset and any number of other
      -- assets.
    | SelectAny
      -- ^ Matches all UTxOs regardless of what assets they contain.
    deriving (Eq, Foldable, Functor, Show, Traversable)

-- | Selects an entry at random from the index according to the given filter.
--
-- Returns the selected entry and an updated index with the entry removed.
--
-- Returns 'Nothing' if there were no matching entries.
--
selectRandom
    :: forall m ctx. (MonadRandom m, SelectionContext ctx)
    => UTxOIndex ctx
    -> SelectionFilter (Asset ctx)
    -> m (Maybe ((UTxO ctx, TokenBundle), UTxOIndex ctx))
selectRandom i selectionFilter =
    (lookupAndRemoveEntry =<<) <$> selectRandomSetMember selectionSet
  where
    lookupAndRemoveEntry
        :: UTxO ctx -> Maybe ((UTxO ctx, TokenBundle), UTxOIndex ctx)
    lookupAndRemoveEntry u =
        (\b -> ((u, b), delete u i)) <$> Map.lookup u (universe i)

    selectionSet :: Set (UTxO ctx)
    selectionSet = case selectionFilter of
        SelectSingleton a ->
            a `lookupWith` indexSingletons
        SelectPairWith a ->
            a `lookupWith` indexPairs
        SelectAnyWith a ->
            a `lookupWith` indexAll
        SelectAny ->
            Map.keysSet (universe i)
      where
        a `lookupWith` index =
            maybe mempty NonEmptySet.toSet $ Map.lookup a $ index i

-- | Selects an entry at random from the index according to the given filters.
--
-- This function traverses the specified list of filters in descending order of
-- priority, from left to right.
--
-- When considering a particular filter:
--
--    - if the function is able to select a UTxO entry that matches, it
--      terminates with that entry and an updated index with the entry removed.
--
--    - if the function is not able to select a UTxO entry that matches, it
--      traverses to the next filter available.
--
-- This function returns 'Nothing' if (and only if) it traverses the entire
-- list of filters without successfully selecting a UTxO entry.
--
selectRandomWithPriority
    :: (MonadRandom m, SelectionContext ctx)
    => UTxOIndex ctx
    -> NonEmpty (SelectionFilter (Asset ctx))
    -- ^ A list of selection filters to be traversed in descending order of
    -- priority, from left to right.
    -> m (Maybe ((UTxO ctx, TokenBundle), UTxOIndex ctx))
selectRandomWithPriority i =
    firstJustM (selectRandom i) . NE.toList

--------------------------------------------------------------------------------
-- Internal Interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Represents different categories of token bundles.
--
data BundleCategory asset
    = BundleWithNoAssets
    | BundleWithOneAsset asset
    | BundleWithTwoAssets (asset, asset)
    | BundleWithMultipleAssets (Set asset)
    deriving (Eq, Show)

-- | Categorizes a token bundle by how many assets it contains.
--
categorizeTokenBundle :: TokenBundle -> BundleCategory asset
categorizeTokenBundle b = undefined {-case F.toList bundleAssets of
    [      ] -> BundleWithNoAssets
    [a     ] -> BundleWithOneAsset a
    [a1, a2] -> BundleWithTwoAssets (a1, a2)
    _        -> BundleWithMultipleAssets bundleAssets
  where
    bundleAssets = undefined -- Asset.tokenBundleAssets b
-}

-- Inserts an entry, but without checking the following pre-condition:
--
-- Pre-condition: there is no existing entry for the specified UTxO identifier.
--
-- See 'insert' for a safe version of this function.
--
insertUnsafe
    :: forall ctx. SelectionContext ctx
    => UTxO ctx
    -> TokenBundle
    -> UTxOIndex ctx
    -> UTxOIndex ctx
insertUnsafe u b i = i
    & over #balance (`TokenBundle.add` b)
    & over #universe (Map.insert u b)
    & case categorizeTokenBundle b of
        BundleWithNoAssets -> id
        BundleWithOneAsset a -> id
            . over #indexAll (`insertEntry` a)
            . over #indexSingletons (`insertEntry` a)
        BundleWithTwoAssets (a1, a2) -> id
            . over #indexAll (`insertEntry` a1)
            . over #indexAll (`insertEntry` a2)
            . over #indexPairs (`insertEntry` a1)
            . over #indexPairs (`insertEntry` a2)
        BundleWithMultipleAssets as -> id
            . over #indexAll (flip (F.foldl' insertEntry) as)
  where
    insertEntry
        :: Map (Asset ctx) (NonEmptySet (UTxO ctx))
        -> Asset ctx
        -> Map (Asset ctx) (NonEmptySet (UTxO ctx))
    insertEntry m a =
        Map.alter (maybe (Just createNew) (Just . updateOld)) a m
      where
        createNew = NonEmptySet.singleton u
        updateOld = NonEmptySet.insert u

-- | Selects an element at random from the given set.
--
-- Returns 'Nothing' if (and only if) the given set is empty.
--
selectRandomSetMember
    :: MonadRandom m
    => Set a
    -> m (Maybe a)
selectRandomSetMember s
    | Set.null s =
        pure Nothing
    | otherwise =
        Just . flip Set.elemAt s <$> getRandomR (0, Set.size s - 1)

--------------------------------------------------------------------------------
-- Invariant
--------------------------------------------------------------------------------

-- | The result of checking the invariant with the 'checkInvariant' function.
--
data InvariantStatus
    = InvariantHolds
      -- ^ Indicates a successful check of the invariant.
    | InvariantBalanceError BalanceError
      -- ^ Indicates that the cached 'balance' value is incorrect.
    | InvariantIndexIncomplete
      -- ^ Indicates that the 'index' is missing one or more entries.
    | InvariantIndexNonMinimal
      -- ^ Indicates that the 'index' has one or more unnecessary entries.
    | InvariantIndexInconsistent
      -- ^ Indicates that the index sets are not consistent.
    | InvariantAssetsInconsistent
      -- ^ Indicates that the 'index' and the cached 'balance' value disagree
      --   about which assets are included.
    deriving (Eq, Show)

-- | Checks whether or not the invariant holds.
--
checkInvariant :: SelectionContext ctx => UTxOIndex ctx -> InvariantStatus
checkInvariant i
    | balanceStatus /= BalanceCorrect =
        InvariantBalanceError balanceError
    | not (indexIsComplete i) =
        InvariantIndexIncomplete
    | not (indexIsMinimal i) =
        InvariantIndexNonMinimal
    | not (indexIsConsistent i) =
        InvariantIndexInconsistent
    | not (assetsConsistent i) =
        InvariantAssetsInconsistent
    | otherwise =
        InvariantHolds
  where
    balanceStatus = checkBalance i
    BalanceIncorrect balanceError = balanceStatus

-- | Indicates whether on not the stored 'balance' value is correct.
--
data BalanceStatus
    = BalanceCorrect
    | BalanceIncorrect BalanceError
    deriving (Eq, Show)

-- | Indicates that the stored 'balance' value is not correct.
--
data BalanceError = BalanceError
    { balanceComputed
        :: TokenBundle
    , balanceStored
        :: TokenBundle
    }
    deriving (Eq, Show)

-- | Checks that calculating the balance from scratch gives a result that
--   is equal to the stored 'balance' value.
--
checkBalance :: UTxOIndex ctx -> BalanceStatus
checkBalance i
    | balanceComputed == balanceStored =
        BalanceCorrect
    | otherwise =
        BalanceIncorrect $ BalanceError {balanceComputed, balanceStored}
  where
    balanceComputed = F.fold (universe i)
    balanceStored = balance i

-- | Checks that every entry in the 'universe' map is properly indexed.
--
indexIsComplete :: forall ctx. SelectionContext ctx => UTxOIndex ctx -> Bool
indexIsComplete i =
    F.all hasEntry $ Map.toList $ universe i
  where
    hasEntry :: (UTxO ctx, TokenBundle) -> Bool
    hasEntry (u, b) = case categorizeTokenBundle b of
        BundleWithNoAssets ->
            True
        BundleWithOneAsset a -> and
            [ hasEntryForAsset a u indexAll
            , hasEntryForAsset a u indexSingletons
            ]
        BundleWithTwoAssets (a1, a2) -> and
            [ hasEntryForAsset a1 u indexAll
            , hasEntryForAsset a2 u indexAll
            , hasEntryForAsset a1 u indexPairs
            , hasEntryForAsset a2 u indexPairs
            ]
        BundleWithMultipleAssets as ->
            F.all (\a -> hasEntryForAsset a u indexAll) as

    hasEntryForAsset
        :: Asset ctx
        -> UTxO ctx
        -> (UTxOIndex ctx -> Map (Asset ctx) (NonEmptySet (UTxO ctx)))
        -> Bool
    hasEntryForAsset asset u assetsMap =
        maybe False (NonEmptySet.member u) $ Map.lookup asset $ assetsMap i

-- | Checks that every indexed entry is required by some entry in the 'universe'
--   map.
--
indexIsMinimal :: forall ctx. SelectionContext ctx => UTxOIndex ctx -> Bool
indexIsMinimal i = F.and
    [ indexAll i
        & Map.toList
        & F.all (\(a, u) -> F.all (entryHasAsset a) u)
    , indexSingletons i
        & Map.toList
        & F.all (\(a, u) -> F.all (entryHasOneAsset a) u)
    , indexPairs i
        & Map.toList
        & F.all (\(a, u) -> F.all (entryHasTwoAssetsWith a) u)
    ]
  where
    entryHasAsset :: Asset ctx -> UTxO ctx -> Bool
    entryHasAsset a = undefined -- entryMatches (`Asset.tokenBundleHasAsset` a)

    entryHasOneAsset :: Asset ctx -> UTxO ctx -> Bool
    entryHasOneAsset a = entryMatches $ \b -> and
        [ undefined -- b `Asset.tokenBundleHasAsset` a
        , undefined -- Asset.tokenBundleAssetCount b == 1
        ]

    entryHasTwoAssetsWith :: Asset ctx -> UTxO ctx -> Bool
    entryHasTwoAssetsWith a = entryMatches $ \b -> and
        [ undefined -- b `Asset.tokenBundleHasAsset` a
        , undefined -- Asset.tokenBundleAssetCount b == 2
        ]

    entryMatches :: (TokenBundle -> Bool) -> UTxO ctx -> Bool
    entryMatches test u = maybe False test $ Map.lookup u $ universe i

-- | Checks that index set relationships are correct.
--
indexIsConsistent :: SelectionContext ctx => UTxOIndex ctx -> Bool
indexIsConsistent i = F.and
    [ indexSingletons i
        `isDisjointTo` indexPairs i
    , indexSingletons i
        `isSubmapOf` indexAll i
    , indexPairs i
        `isSubmapOf` indexAll i
    ]
  where
    isDisjointTo
        :: Ord u
        => Map a (NonEmptySet u)
        -> Map a (NonEmptySet u)
        -> Bool
    isDisjointTo m1 m2 = s1 `Set.disjoint` s2
      where
        s1 = F.foldMap NonEmptySet.toSet m1
        s2 = F.foldMap NonEmptySet.toSet m2

    isSubmapOf
        :: (Ord a, Ord u)
        => Map a (NonEmptySet u)
        -> Map a (NonEmptySet u)
        -> Bool
    isSubmapOf m1 m2 = Map.isSubmapOfBy isNonEmptySubsetOf m1 m2
      where
        isNonEmptySubsetOf s1 s2 =
            NonEmptySet.toSet s1 `Set.isSubsetOf` NonEmptySet.toSet s2

-- | Checks that the asset sets are consistent.
--
-- In particular, the set of assets in the cached 'balance' must be:
--
--    - equal to the set of assets in 'indexAll'
--    - a superset of the set of assets in 'indexSingletons'.
--    - a superset of the set of assets in 'indexPairs'.
--
assetsConsistent :: SelectionContext ctx => UTxOIndex ctx -> Bool
assetsConsistent i = and
    [ Map.keysSet (indexAll i)
        == balanceAssets
    , Map.keysSet (indexSingletons i)
        `Set.isSubsetOf` balanceAssets
    , Map.keysSet (indexPairs i)
        `Set.isSubsetOf` balanceAssets
    ]
  where
    balanceAssets = undefined -- Asset.tokenBundleAssets (balance i)
