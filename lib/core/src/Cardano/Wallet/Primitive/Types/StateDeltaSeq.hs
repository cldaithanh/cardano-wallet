{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq
    , append
    , appendMany
    , fromState
    , headState
    , lastState
    , isPrefixOf
    , isSuffixOf
    , dropHead
    , dropHeads
    , dropLast
    , dropLasts
    , mergeHead
    , mergeHeads
    , mergeLast
    , mergeLasts
    , size
    , toDeltaList
    , toStateList
    ) where

import Prelude hiding
    ( head, iterate, seq, tail )

import Control.Monad
    ( foldM )
import Data.Functor
    ( (<&>) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Vector
    ( Vector, (!) )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Vector.Extra as V

data StateDeltaSeq state delta = StateDeltaSeq
    { head :: state
    , tail :: Vector (delta, state)
    }
    deriving (Eq, Show)

headState :: StateDeltaSeq state delta -> state
headState StateDeltaSeq {head} = head

lastState :: StateDeltaSeq state delta -> state
lastState StateDeltaSeq {head, tail}
    | null tail = head
    | otherwise = snd (V.last tail)

size :: StateDeltaSeq state delta -> Int
size = length . tail

fromState :: state -> StateDeltaSeq state delta
fromState state = StateDeltaSeq state V.empty

toDeltaList :: StateDeltaSeq state delta -> [delta]
toDeltaList = fmap fst . F.toList . tail

toStateList :: StateDeltaSeq state delta -> NonEmpty state
toStateList StateDeltaSeq {head, tail} =
    head :| (snd <$> F.toList tail)

append
    :: Functor m
    => (state -> delta -> m state)
    -> StateDeltaSeq state delta
    -> delta
    -> m (StateDeltaSeq state delta)
append nextState seq@StateDeltaSeq {head, tail} delta =
    nextState (lastState seq) delta <&> \state -> StateDeltaSeq
        {head, tail = tail `V.snoc` (delta, state)}

appendMany
    :: (Foldable f, Monad m)
    => (state -> delta -> m state)
    -> StateDeltaSeq state delta
    -> f delta
    -> m (StateDeltaSeq state delta)
appendMany = foldM . append

iterate
    :: (StateDeltaSeq state delta -> Maybe (StateDeltaSeq state delta))
    -> StateDeltaSeq state delta
    -> NonEmpty (StateDeltaSeq state delta)
iterate transform seq0 =
    loop (seq0 :| []) seq0
  where
    loop !acc !seq = maybe acc (\p -> loop (p `NE.cons` acc) p) (transform seq)

dropHead
    :: StateDeltaSeq state delta
    -> Maybe (StateDeltaSeq state delta)
dropHead StateDeltaSeq {tail}
    | null tail = Nothing
    | otherwise = Just StateDeltaSeq
        {head = snd $ V.head tail, tail = V.dropHead 1 tail}

dropLast
    :: StateDeltaSeq state delta
    -> Maybe (StateDeltaSeq state delta)
dropLast StateDeltaSeq {head, tail}
    | null tail = Nothing
    | otherwise = Just StateDeltaSeq
        {head, tail = V.dropLast 1 tail}

dropHeads :: StateDeltaSeq state delta -> NonEmpty (StateDeltaSeq state delta)
dropHeads = iterate dropHead

dropLasts :: StateDeltaSeq state delta -> NonEmpty (StateDeltaSeq state delta)
dropLasts = iterate dropLast

-- Performs the following transformation:
--
--    state_0 : delta_0_1 : state_1 : delta_1_2 : state_2 : ...
--    state_0 : delta_0_1 +           delta_1_2 : state_2 : ...
--
mergeHead
    :: (delta -> delta -> delta)
    -> StateDeltaSeq state delta
    -> Maybe (StateDeltaSeq state delta)
mergeHead merge StateDeltaSeq {head, tail}
    | length tail < 2 = Nothing
    | otherwise = Just StateDeltaSeq
        { head
        , tail = (d_0_1 `merge` d_1_2, s_2) `V.cons` V.dropHead 2 tail
        }
  where
    (d_0_1, _s_1) = tail ! 0
    (d_1_2,  s_2) = tail ! 1

-- Performs the following transformation:
--
--    ... : state_2 : delta_2_1 : state_1 : delta_1_0 : state_0
--    ... : state_2 : delta_2-1 +         : delta_1_0 : state_0
--
mergeLast
    :: (delta -> delta -> delta)
    -> StateDeltaSeq state delta
    -> Maybe (StateDeltaSeq state delta)
mergeLast merge StateDeltaSeq {head, tail}
    | length tail < 2 = Nothing
    | otherwise = Just StateDeltaSeq
        { head
        , tail = V.dropLast 2 tail `V.snoc` (d_2_1 `merge` d_1_0, s_0)
        }
  where
    (d_1_0,  s_0) = tail ! (length tail - 1)
    (d_2_1, _s_1) = tail ! (length tail - 2)

mergeHeads
    :: (delta -> delta -> delta)
    -> StateDeltaSeq state delta
    -> NonEmpty (StateDeltaSeq state delta)
mergeHeads = iterate . mergeHead

mergeLasts
    :: (delta -> delta -> delta)
    -> StateDeltaSeq state delta
    -> NonEmpty (StateDeltaSeq state delta)
mergeLasts = iterate . mergeLast

isPrefixOf
    :: (Eq state, Eq delta)
    => StateDeltaSeq state delta
    -> StateDeltaSeq state delta
    -> Bool
isPrefixOf s1 s2 = (&&)
    (toDeltaList s1  `L.isPrefixOf` toDeltaList s2)
    (NE.toList (toStateList s1) `L.isPrefixOf` NE.toList (toStateList s2))

isSuffixOf
    :: (Eq state, Eq delta)
    => StateDeltaSeq state delta
    -> StateDeltaSeq state delta
    -> Bool
isSuffixOf s1 s2 = (&&)
    (toDeltaList s1  `L.isSuffixOf` toDeltaList s2)
    (NE.toList (toStateList s1) `L.isSuffixOf` NE.toList (toStateList s2))
