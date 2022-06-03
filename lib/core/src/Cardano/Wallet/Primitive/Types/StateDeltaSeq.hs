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
    , isValid
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

headState :: StateDeltaSeq s d -> s
headState StateDeltaSeq {head} = head

lastState :: StateDeltaSeq s d -> s
lastState StateDeltaSeq {head, tail}
    | null tail = head
    | otherwise = snd (V.last tail)

size :: StateDeltaSeq s d -> Int
size = length . tail

fromState :: s -> StateDeltaSeq s d
fromState state = StateDeltaSeq state V.empty

toDeltaList :: StateDeltaSeq s d -> [d]
toDeltaList = fmap fst . F.toList . tail

toStateList :: StateDeltaSeq s d -> NonEmpty s
toStateList StateDeltaSeq {head, tail} = head :| (snd <$> F.toList tail)

append
    :: Functor m
    => (s -> d -> m s)
    -> StateDeltaSeq s d
    -> d
    -> m (StateDeltaSeq s d)
append nextState seq@StateDeltaSeq {head, tail} delta =
    nextState (lastState seq) delta <&> \state -> StateDeltaSeq
        {head, tail = tail `V.snoc` (delta, state)}

appendMany
    :: (Foldable f, Monad m)
    => (s -> d -> m s)
    -> StateDeltaSeq s d
    -> f d
    -> m (StateDeltaSeq s d)
appendMany = foldM . append

iterate
    :: (StateDeltaSeq s d -> Maybe (StateDeltaSeq s d))
    -> StateDeltaSeq s d
    -> NonEmpty (StateDeltaSeq s d)
iterate transform seq0 =
    loop (seq0 :| []) seq0
  where
    loop !acc !seq = maybe acc (\p -> loop (p `NE.cons` acc) p) (transform seq)

dropHead :: StateDeltaSeq s d -> Maybe (StateDeltaSeq s d)
dropHead StateDeltaSeq {tail}
    | null tail = Nothing
    | otherwise = Just StateDeltaSeq
        {head = snd $ V.head tail, tail = V.dropHead 1 tail}

dropLast :: StateDeltaSeq s d -> Maybe (StateDeltaSeq s d)
dropLast StateDeltaSeq {head, tail}
    | null tail = Nothing
    | otherwise = Just StateDeltaSeq
        {head, tail = V.dropLast 1 tail}

-- Performs the following transformation:
--
--    state_0 : delta_0_1 : state_1 : delta_1_2 : state_2 : ...
--    state_0 : delta_0_1 +           delta_1_2 : state_2 : ...
--    state_0 : delta_0_2                       : state_2 : ...
--
mergeHead :: (d -> d -> d) -> StateDeltaSeq s d -> Maybe (StateDeltaSeq s d)
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
--    ... : state_2 : delta_2_1 +           delta_1_0 : state_0
--    ... : state_2 : delta_2_0                       : state_0
--
mergeLast :: (d -> d -> d) -> StateDeltaSeq s d -> Maybe (StateDeltaSeq s d)
mergeLast merge StateDeltaSeq {head, tail}
    | length tail < 2 = Nothing
    | otherwise = Just StateDeltaSeq
        { head
        , tail = V.dropLast 2 tail `V.snoc` (d_2_1 `merge` d_1_0, s_0)
        }
  where
    (d_2_1, _s_1) = tail ! (length tail - 2)
    (d_1_0,  s_0) = tail ! (length tail - 1)

dropHeads :: StateDeltaSeq s d -> NonEmpty (StateDeltaSeq s d)
dropHeads = iterate dropHead

dropLasts :: StateDeltaSeq s d -> NonEmpty (StateDeltaSeq s d)
dropLasts = iterate dropLast

mergeHeads :: (d -> d -> d) -> StateDeltaSeq s d -> NonEmpty (StateDeltaSeq s d)
mergeHeads = iterate . mergeHead

mergeLasts :: (d -> d -> d) -> StateDeltaSeq s d -> NonEmpty (StateDeltaSeq s d)
mergeLasts = iterate . mergeLast

isPrefixOf :: (Eq s, Eq d) => StateDeltaSeq s d -> StateDeltaSeq s d -> Bool
isPrefixOf s1 s2 = (&&)
    (toDeltaList s1  `L.isPrefixOf` toDeltaList s2)
    (NE.toList (toStateList s1) `L.isPrefixOf` NE.toList (toStateList s2))

isSuffixOf :: (Eq s, Eq d) => StateDeltaSeq s d -> StateDeltaSeq s d -> Bool
isSuffixOf s1 s2 = (&&)
    (toDeltaList s1  `L.isSuffixOf` toDeltaList s2)
    (NE.toList (toStateList s1) `L.isSuffixOf` NE.toList (toStateList s2))

isValid :: (s -> d -> Maybe s) -> StateDeltaSeq s d -> Bool
isValid seq nextState = undefined
