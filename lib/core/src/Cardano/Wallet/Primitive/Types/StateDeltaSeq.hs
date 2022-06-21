{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeq
    (
    -- * Type
      StateDeltaSeq

    -- * Constructors
    , fromState
    , fromStateDeltas

    -- * Views
    , headState
    , lastState
    , toDeltaList
    , toStateList
    , transitions
    , countEmptyTransitions
    , countEmptyTransitionsWhere

    -- * Expansions
    , applyDelta
    , applyDeltas
    , applyDeltaM
    , applyDeltasM

    -- * Reductions
    , dropEmptyTransition
    , dropEmptyTransitions
    , dropEmptyTransitionWhere
    , dropEmptyTransitionsWhere
    , dropHead
    , dropHeads
    , dropLast
    , dropLasts

    -- * Transformations
    , mapDeltas
    , mapStates
    , mapStatesDeltas

    -- * Queries
    , isPrefixOf
    , isSuffixOf
    , isValid
    , isValidM

    ) where

import Prelude hiding
    ( head, iterate, seq, tail )

import Control.Applicative
    ( ZipList (..) )
import Control.Monad
    ( foldM )
import Control.Monad.Extra
    ( allM )
import Control.Monad.Identity
    ( Identity (..) )
import Data.Bifoldable
    ( Bifoldable (..) )
import Data.Bifunctor
    ( Bifunctor (..) )
import Data.Coerce
    ( coerce )
import Data.Function
    ( on )
import Data.Functor
    ( (<&>) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Sequence
    ( Seq ((:<|), (:|>), Empty) )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data StateDeltaSeq state delta = StateDeltaSeq
    { head :: !state
    , tail :: !(Seq (delta, state))
    }
    deriving Eq

data StateDeltaListItem state delta
    = State !state
    | Delta !delta
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Bifoldable StateDeltaSeq where
    bifoldMap f g s = head <> F.foldMap (uncurry (<>)) tail
      where
        StateDeltaSeq {head, tail} = mapStatesDeltas f g s

instance Bifunctor StateDeltaSeq where
    bimap = mapStatesDeltas
    first = mapStates
    second = mapDeltas

instance Foldable (StateDeltaSeq state) where
    foldMap f s = F.foldMap f (toDeltaList s)

instance (Show state, Show delta) => Show (StateDeltaSeq state delta) where
    show = show . NE.toList . toStateDeltaList

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

fromState :: s -> StateDeltaSeq s d
fromState state = StateDeltaSeq state Seq.empty

fromStateDeltas :: (s -> d -> s) -> s -> [d] -> StateDeltaSeq s d
fromStateDeltas next s ds = applyDeltas next ds (fromState s)

--------------------------------------------------------------------------------
-- Views
--------------------------------------------------------------------------------

headState :: StateDeltaSeq s d -> s
headState StateDeltaSeq {head} = head

lastState :: StateDeltaSeq s d -> s
lastState StateDeltaSeq {head, tail} = case tail of
    Empty -> head
    _ :|> (_, s) -> s

toDeltaList :: StateDeltaSeq s d -> [d]
toDeltaList = fmap fst . F.toList . tail

toStateList :: StateDeltaSeq s d -> NonEmpty s
toStateList StateDeltaSeq {head, tail} = head :| (snd <$> F.toList tail)

toStateDeltaList :: StateDeltaSeq s d -> NonEmpty (StateDeltaListItem s d)
toStateDeltaList s = NE.fromList $ interleave
    (State <$> F.toList (toStateList s))
    (Delta <$> F.toList (toDeltaList s))

transitions :: StateDeltaSeq s d -> [(s, d, s)]
transitions s = getZipList $ (,,)
    <$> ZipList states
    <*> ZipList deltas
    <*> ZipList (drop 1 states)
  where
    deltas = F.toList $ toDeltaList s
    states = F.toList $ toStateList s

countEmptyTransitions :: Eq s => StateDeltaSeq s d -> Int
countEmptyTransitions = countEmptyTransitionsWhere (const True)

countEmptyTransitionsWhere :: Eq s => (d -> Bool) -> StateDeltaSeq s d -> Int
countEmptyTransitionsWhere f s = length $ emptyTransitionsWhere f s

--------------------------------------------------------------------------------
-- Expansions
--------------------------------------------------------------------------------

applyDelta :: (s -> d -> s) -> d -> StateDeltaSeq s d -> StateDeltaSeq s d
applyDelta next delta = runIdentity . applyDeltaM (coerce next) delta

applyDeltaM
    :: Functor m
    => (s -> d -> m s)
    -> d
    -> StateDeltaSeq s d
    -> m (StateDeltaSeq s d)
applyDeltaM next delta seq@StateDeltaSeq {head, tail} =
    next (lastState seq) delta <&> \state ->
        StateDeltaSeq {head, tail = tail :|> (delta, state)}

applyDeltas
    :: Foldable f
    => (s -> d -> s)
    -> f d
    -> StateDeltaSeq s d
    -> StateDeltaSeq s d
applyDeltas next deltas seq = F.foldl' (flip (applyDelta next)) seq deltas

applyDeltasM
    :: (Foldable f, Monad m)
    => (s -> d -> m s)
    -> f d
    -> StateDeltaSeq s d
    -> m (StateDeltaSeq s d)
applyDeltasM next deltas seq = foldM (flip (applyDeltaM next)) seq deltas

--------------------------------------------------------------------------------
-- Reductions
--------------------------------------------------------------------------------

dropEmptyTransition
    :: Eq s => StateDeltaSeq s d -> [StateDeltaSeq s d]
dropEmptyTransition = dropEmptyTransitionWhere (const True)

dropEmptyTransitions
    :: Eq s => StateDeltaSeq s d -> StateDeltaSeq s d
dropEmptyTransitions = dropEmptyTransitionsWhere (const True)

dropEmptyTransitionWhere
    :: Eq s => (d -> Bool) -> StateDeltaSeq s d -> [StateDeltaSeq s d]
dropEmptyTransitionWhere f s@StateDeltaSeq {head, tail} =
    StateDeltaSeq head . flip Seq.deleteAt tail <$> emptyTransitionsWhere f s

dropEmptyTransitionsWhere
    :: Eq s => (d -> Bool) -> StateDeltaSeq s d -> StateDeltaSeq s d
dropEmptyTransitionsWhere f s@StateDeltaSeq {head, tail} = StateDeltaSeq head $
    F.foldl' (flip Seq.deleteAt) tail (reverse $ emptyTransitionsWhere f s)

dropHead :: StateDeltaSeq s d -> Maybe (StateDeltaSeq s d)
dropHead StateDeltaSeq {tail} = case tail of
    Empty -> Nothing
    (_, head) :<| xs -> Just StateDeltaSeq {head, tail = xs}

dropHeads :: StateDeltaSeq s d -> [StateDeltaSeq s d]
dropHeads = iterateMaybe dropHead

dropLast :: StateDeltaSeq s d -> Maybe (StateDeltaSeq s d)
dropLast StateDeltaSeq {head, tail} = case tail of
    Empty -> Nothing
    xs :|> _ -> Just StateDeltaSeq {head, tail = xs}

dropLasts :: StateDeltaSeq s d -> [StateDeltaSeq s d]
dropLasts = iterateMaybe dropLast

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

mapDeltas :: (d1 -> d2) -> StateDeltaSeq s d1 -> StateDeltaSeq s d2
mapDeltas f StateDeltaSeq {head, tail} = StateDeltaSeq
    {head, tail = first f <$> tail}

mapStates :: (s1 -> s2) -> StateDeltaSeq s1 d -> StateDeltaSeq s2 d
mapStates f StateDeltaSeq {head, tail} = StateDeltaSeq
    {head = f head, tail = second f <$> tail}

mapStatesDeltas
    :: (s1 -> s2) -> (d1 -> d2) -> StateDeltaSeq s1 d1 -> StateDeltaSeq s2 d2
mapStatesDeltas f g StateDeltaSeq {head, tail} = StateDeltaSeq
    {head = f head, tail = bimap g f <$> tail}

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

isPrefixOf :: (Eq s, Eq d) => StateDeltaSeq s d -> StateDeltaSeq s d -> Bool
isPrefixOf = L.isPrefixOf `on` transitions

isSuffixOf :: (Eq s, Eq d) => StateDeltaSeq s d -> StateDeltaSeq s d -> Bool
isSuffixOf = L.isSuffixOf `on` transitions

isValid :: (Eq s) => (s -> d -> s) -> StateDeltaSeq s d -> Bool
isValid next = runIdentity . isValidM (coerce next)

isValidM :: (Monad m, Eq s) => (s -> d -> m s) -> StateDeltaSeq s d -> m Bool
isValidM next = allM (\(si, d, sj) -> (==) sj <$> next si d) . transitions

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

emptyTransitionsWhere :: Eq s => (d -> Bool) -> StateDeltaSeq s d -> [Int]
emptyTransitionsWhere f s =
    fst <$> filter (isEmptyTransitionWhere f . snd) (zip [0 ..] (transitions s))

isEmptyTransitionWhere :: Eq s => (d -> Bool) -> (s, d, s) -> Bool
isEmptyTransitionWhere f (si, d, sj) = si == sj && f d

interleave :: [a] -> [a] -> [a]
interleave (a1 : a1s) (a2 : a2s) = a1 : a2 : interleave a1s a2s
interleave (     a1s) [        ] = a1s
interleave [        ] (     a2s) = a2s

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f =
    loop []
  where
    loop !as !a = maybe as (\p -> loop (p : as) p) (f a)
