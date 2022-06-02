{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq
    , append
    , appendMany
    , initialState
    , finalState
    , fromInitialState
    , isPrefixOf
    , prefixes
    , size
    , toDeltaList
    ) where

import Prelude hiding
    ( seq )

import Control.Monad
    ( foldM )
import Data.Functor
    ( (<&>) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Maybe
    ( listToMaybe )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

data StateDeltaSeq state delta = StateDeltaSeq
    { initialState :: state
    , deltaStates :: [(delta, state)]
    }
    deriving (Eq, Show)

finalState :: StateDeltaSeq state delta -> state
finalState StateDeltaSeq {initialState, deltaStates} =
    maybe initialState snd (listToMaybe deltaStates)

size :: StateDeltaSeq state delta -> Int
size = length . deltaStates

fromInitialState :: state -> StateDeltaSeq state delta
fromInitialState state = StateDeltaSeq state []

toDeltaList :: StateDeltaSeq state delta -> [delta]
toDeltaList = reverse . fmap fst . deltaStates

append
    :: Functor m
    => (state -> delta -> m state)
    -> StateDeltaSeq state delta
    -> delta
    -> m (StateDeltaSeq state delta)
append nextState seq delta =
    nextState (finalState seq) delta <&> \state ->
        StateDeltaSeq (initialState seq) ((delta, state) : deltaStates seq)

appendMany
    :: forall f m state delta. (Foldable f, Monad m)
    => (state -> delta -> m state)
    -> StateDeltaSeq state delta
    -> f delta
    -> m (StateDeltaSeq state delta)
appendMany = foldM . append

prefixes :: StateDeltaSeq state delta -> NonEmpty (StateDeltaSeq state delta)
prefixes seq = StateDeltaSeq (initialState seq) <$>
    NE.reverse (NE.tails (deltaStates seq))

isPrefixOf
    :: (Eq state, Eq delta)
    => StateDeltaSeq state delta
    -> StateDeltaSeq state delta
    -> Bool
isPrefixOf s1 s2 = (&&)
    (initialState s1 == initialState s2)
    (deltaStates s1 `L.isSuffixOf` deltaStates s2)
