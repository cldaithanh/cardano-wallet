{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq
    , unfoldNM
    , transitions
    , applyDelta
    , applyDeltas
    , applyDeltaM
    , applyDeltasM
    , fromState
    , headState
    , lastState
    , isPrefixOf
    , isSuffixOf
    , isValid
    , isValidM
    , dropHead
    , dropHeads
    , dropLast
    , dropLasts
    , toDeltaList
    , toStateList
    ) where

import Prelude hiding
    ( head, iterate, seq, tail )

import Control.Monad
    ( foldM )
import Control.Monad.Identity
    ( Identity (..) )
import Data.Bifoldable
    ( Bifoldable (..) )
import Data.Bifunctor
    ( Bifunctor (..) )
import Data.Function
    ( on )
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
import qualified Data.Vector.Extra as V

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data StateDeltaSeq state delta = StateDeltaSeq
    { head :: !state
    , tail :: !(Vector (delta, state))
    }
    deriving Eq

data StateOrDelta state delta
    = State !state
    | Delta !delta
    deriving (Eq, Show)

type ApplyDelta state delta = state -> delta -> state
type ApplyDeltaM m state delta = state -> delta -> m state

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
-- Operations
--------------------------------------------------------------------------------

transitions :: forall s d. StateDeltaSeq s d -> [(s, d, s)]
transitions StateDeltaSeq {head, tail}
    | V.length tail == 0 =
        []
    | otherwise =
        (head, fst (V.head tail), snd (V.head tail)) : rest
  where
    rest :: [(s, d, s)]
    rest = zip (F.toList tail) (drop 1 $ F.toList tail)
        <&> \((_d0, s0), (d1, s1)) -> (s0, d1, s1)

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
        | otherwise = loop (j - 1)
            =<< applyDeltaM nextState seq
            =<< nextDelta (lastState seq)

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

headState :: StateDeltaSeq s d -> s
headState StateDeltaSeq {head} = head

lastState :: StateDeltaSeq s d -> s
lastState StateDeltaSeq {head, tail}
    | null tail = head
    | otherwise = snd (V.last tail)

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
    -> [StateDeltaSeq s d]
iterate transform =
    loop []
  where
    loop !acc !seq = maybe acc (\p -> loop (p : acc) p) (transform seq)

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

dropHeads :: StateDeltaSeq s d -> [StateDeltaSeq s d]
dropHeads = iterate dropHead

dropLasts :: StateDeltaSeq s d -> [StateDeltaSeq s d]
dropLasts = iterate dropLast

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
