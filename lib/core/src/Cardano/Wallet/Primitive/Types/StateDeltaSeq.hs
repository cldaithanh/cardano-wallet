{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq
    , unfoldNM
    , applyDelta
    , applyDeltas
    , applyDeltaM
    , applyDeltasM
    , fromState
    , headState
    , lastState
    , isEquivalentTo
    , isPrefixOf
    , isSuffixOf
    , isValid
    , isValidM
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
import Control.Monad.Identity
    ( Identity (..) )
import Data.Bifunctor
    ( Bifunctor (..) )
import Data.Function
    ( on )
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

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data StateDeltaSeq state delta = StateDeltaSeq
    { head :: state
    , tail :: Vector (delta, state)
    }
    deriving Eq

data StateOrDelta state delta
    = State state
    | Delta delta
    deriving (Eq, Show)

type ApplyDelta state delta = state -> delta -> state
type ApplyDeltaM m state delta = state -> delta -> m state
type MergeDelta delta = delta -> delta -> delta

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Bifunctor StateDeltaSeq where
    first = mapStates
    second = mapDeltas

instance (Show state, Show delta) => Show (StateDeltaSeq state delta) where
    show = show . NE.toList . toStateDeltaList

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

unfoldNM
    :: Monad m
    => Int
    -> (s -> m d)
    -> (s -> d -> m s)
    -> s
    -> m (StateDeltaSeq s d)
unfoldNM i nextDelta nextState startState = loop i (fromState startState)
  where
    loop !j !seq
        | j <= 0 = pure seq
        | otherwise = do
            d <- nextDelta (lastState seq)
            seq' <- applyDeltaM nextState seq d
            loop (j - 1) seq'

mapDeltas :: (d1 -> d2) -> StateDeltaSeq s d1 -> StateDeltaSeq s d2
mapDeltas f StateDeltaSeq {head, tail} = StateDeltaSeq
    {head, tail = first f <$> tail}

mapStates :: (s1 -> s2) -> StateDeltaSeq s1 d -> StateDeltaSeq s2 d
mapStates f StateDeltaSeq {head, tail} = StateDeltaSeq
    {head = f head, tail = second f <$> tail}

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

toStateDeltaList :: StateDeltaSeq s d -> NonEmpty (StateOrDelta s d)
toStateDeltaList s = NE.fromList $ interleave
    (State <$> F.toList (toStateList s))
    (Delta <$> F.toList (toDeltaList s))

toDeltaList :: StateDeltaSeq s d -> [d]
toDeltaList = fmap fst . F.toList . tail

toStateList :: StateDeltaSeq s d -> NonEmpty s
toStateList StateDeltaSeq {head, tail} = head :| (snd <$> F.toList tail)

applyDelta :: ApplyDelta s d -> StateDeltaSeq s d -> d -> StateDeltaSeq s d
applyDelta = ((runIdentity .) .) . applyDeltaM . (fmap Identity <$>)

applyDeltas
    :: Foldable f
    => ApplyDelta s d
    -> StateDeltaSeq s d
    -> f d
    -> StateDeltaSeq s d
applyDeltas = F.foldl' . applyDelta

applyDeltaM
    :: Functor m
    => ApplyDeltaM m s d
    -> StateDeltaSeq s d
    -> d
    -> m (StateDeltaSeq s d)
applyDeltaM nextState seq@StateDeltaSeq {head, tail} delta =
    nextState (lastState seq) delta <&> \state -> StateDeltaSeq
        {head, tail = tail `V.snoc` (delta, state)}

applyDeltasM
    :: (Foldable f, Monad m)
    => ApplyDeltaM m s d
    -> StateDeltaSeq s d
    -> f d
    -> m (StateDeltaSeq s d)
applyDeltasM = foldM . applyDeltaM

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
mergeHead :: MergeDelta d -> StateDeltaSeq s d -> Maybe (StateDeltaSeq s d)
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
mergeLast :: MergeDelta d -> StateDeltaSeq s d -> Maybe (StateDeltaSeq s d)
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

mergeHeads :: MergeDelta d -> StateDeltaSeq s d -> NonEmpty (StateDeltaSeq s d)
mergeHeads = iterate . mergeHead

mergeLasts :: MergeDelta d -> StateDeltaSeq s d -> NonEmpty (StateDeltaSeq s d)
mergeLasts = iterate . mergeLast

isEquivalentTo :: Eq s => StateDeltaSeq s d -> StateDeltaSeq s d -> Bool
isEquivalentTo = (==) `on` ((,) <$> headState <*> lastState)

isPrefixOf :: (Eq s, Eq d) => StateDeltaSeq s d -> StateDeltaSeq s d -> Bool
isPrefixOf = L.isPrefixOf `on` F.toList . toStateDeltaList

isSuffixOf :: (Eq s, Eq d) => StateDeltaSeq s d -> StateDeltaSeq s d -> Bool
isSuffixOf = L.isSuffixOf `on` F.toList . toStateDeltaList

isValid :: (Eq s, Eq d) => ApplyDelta s d -> StateDeltaSeq s d -> Bool
isValid = ((Just True ==) .) . isValidM . (fmap Just <$>)

isValidM
    :: forall m s d. (Monad m, Eq s, Eq d)
    => ApplyDeltaM m s d
    -> StateDeltaSeq s d
    -> m Bool
isValidM nextState seq@StateDeltaSeq {head} = (==)
    <$> applyDeltasM nextState (fromState head) (toDeltaList seq)
    <*> pure seq

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

interleave :: [a] -> [a] -> [a]
interleave (a1 : a1s) (a2 : a2s) = a1 : a2 : interleave a1s a2s
interleave (     a1s) [        ] = a1s
interleave [        ] (     a2s) = a2s
