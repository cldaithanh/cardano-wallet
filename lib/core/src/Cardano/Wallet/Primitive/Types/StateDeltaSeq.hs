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
    , dropHead
    , dropLast
    , isPrefixOf
    , isSuffixOf
    , prefixes
    , suffixes
    , size
    , toDeltaList
    , toStateList
    ) where

import Prelude hiding
    ( seq )

import Control.Monad
    ( foldM )
import Data.Functor
    ( (<&>) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Vector
    ( Vector )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

data StateDeltaSeq state delta = StateDeltaSeq
    { headState :: state
    , deltas :: Vector (delta, state)
    }
    deriving (Eq, Show)

lastState :: StateDeltaSeq state delta -> state
lastState StateDeltaSeq {headState, deltas}
    | null deltas = headState
    | otherwise = snd (V.last deltas)

size :: StateDeltaSeq state delta -> Int
size = length . deltas

fromState :: state -> StateDeltaSeq state delta
fromState state = StateDeltaSeq state V.empty

toDeltaList :: StateDeltaSeq state delta -> [delta]
toDeltaList = fmap fst . F.toList . deltas

toStateList :: StateDeltaSeq state delta -> NonEmpty state
toStateList StateDeltaSeq {headState, deltas} =
    headState :| (snd <$> F.toList deltas)

append
    :: Functor m
    => (state -> delta -> m state)
    -> StateDeltaSeq state delta
    -> delta
    -> m (StateDeltaSeq state delta)
append nextState seq@StateDeltaSeq {headState, deltas} delta =
    nextState (lastState seq) delta <&> \state -> StateDeltaSeq
        {headState, deltas = deltas `V.snoc` (delta, state)}

appendMany
    :: (Foldable f, Monad m)
    => (state -> delta -> m state)
    -> StateDeltaSeq state delta
    -> f delta
    -> m (StateDeltaSeq state delta)
appendMany = foldM . append

prefixes :: StateDeltaSeq state delta -> NonEmpty (StateDeltaSeq state delta)
prefixes seq0 =
    loop (seq0 :| []) seq0
  where
    loop !acc !seq = maybe acc (\p -> loop (p `NE.cons` acc) p) (dropHead seq)

suffixes :: StateDeltaSeq state delta -> NonEmpty (StateDeltaSeq state delta)
suffixes seq0 =
    loop (seq0 :| []) seq0
  where
    loop !acc !seq = maybe acc (\s -> loop (s `NE.cons` acc) s) (dropLast seq)

dropHead
    :: StateDeltaSeq state delta
    -> Maybe (StateDeltaSeq state delta)
dropHead StateDeltaSeq {headState, deltas}
    | null deltas = Nothing
    | otherwise = Just StateDeltaSeq
        {headState, deltas = V.take (length deltas - 1) deltas}

dropLast
    :: StateDeltaSeq state delta
    -> Maybe (StateDeltaSeq state delta)
dropLast StateDeltaSeq {deltas}
    | null deltas = Nothing
    | otherwise = Just StateDeltaSeq
        {headState = snd $ V.head deltas, deltas = V.drop 1 deltas}

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
