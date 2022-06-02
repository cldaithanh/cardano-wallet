{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq
    , append
    , appendMany
    , fromInitialState
    , finalState
    , initialState
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
import Data.Vector
    ( Vector )
import Data.List.NonEmpty
    ( NonEmpty (..) )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

data StateDeltaSeq state delta = StateDeltaSeq
    { initialState :: state
    , deltaStates :: Vector (delta, state)
    }
    deriving (Eq, Show)

finalState :: StateDeltaSeq state delta -> state
finalState StateDeltaSeq {initialState, deltaStates}
    | null deltaStates =
        initialState
    | otherwise =
        snd (V.last deltaStates)

size :: StateDeltaSeq state delta -> Int
size = length . deltaStates

fromInitialState :: state -> StateDeltaSeq state delta
fromInitialState state = StateDeltaSeq state V.empty

toDeltaList :: StateDeltaSeq state delta -> [delta]
toDeltaList = fmap fst . F.toList . deltaStates

toStateList :: StateDeltaSeq state delta -> NonEmpty state
toStateList StateDeltaSeq {initialState, deltaStates} =
    initialState :| (snd <$> F.toList deltaStates)

append
    :: Functor m
    => (state -> delta -> m state)
    -> StateDeltaSeq state delta
    -> delta
    -> m (StateDeltaSeq state delta)
append nextState seq@StateDeltaSeq {initialState, deltaStates} delta =
    nextState (finalState seq) delta <&> \state ->
        StateDeltaSeq
            { initialState
            , deltaStates = deltaStates `V.snoc` (delta, state)
            }

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
    loop !acc !seq = case longestProperPrefix seq of
        Just prefix -> loop (NE.cons prefix acc) prefix
        Nothing -> acc

suffixes :: StateDeltaSeq state delta -> NonEmpty (StateDeltaSeq state delta)
suffixes seq0 =
    loop (seq0 :| []) seq0
  where
    loop !acc !seq = case longestProperSuffix seq of
        Just suffix -> loop (NE.cons suffix acc) suffix
        Nothing -> acc

longestProperPrefix
    :: StateDeltaSeq state delta
    -> Maybe (StateDeltaSeq state delta)
longestProperPrefix StateDeltaSeq {initialState, deltaStates}
    | null deltaStates = Nothing
    | otherwise = Just StateDeltaSeq
        { initialState
        , deltaStates = V.take (length deltaStates - 1) deltaStates
        }

longestProperSuffix
    :: StateDeltaSeq state delta
    -> Maybe (StateDeltaSeq state delta)
longestProperSuffix StateDeltaSeq {deltaStates}
    | null deltaStates = Nothing
    | otherwise = Just StateDeltaSeq
        { initialState = snd $ V.head deltaStates
        , deltaStates = V.drop 1 deltaStates
        }

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
