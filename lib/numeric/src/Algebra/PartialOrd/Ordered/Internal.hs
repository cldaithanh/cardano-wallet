{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}

module Algebra.PartialOrd.Ordered.Internal
    ( MaybeOrdered (..)
    , AsList (..)
    , Ordered
    , mapAsList
    , ordered
    ) where

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Algebra.PartialOrd.Operators
    ( PartialOrdOperators (..) )
import Control.Monad
    ( (<=<) )
import Data.List.NonEmpty
    ( NonEmpty )
import Prelude hiding
    ( Ord (..) )
import Safe
    ( tailMay )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

-- Remove the sorting! We don't need it.
-- We just need to check that it's in order.
--
--
-- Actually, it's only necessary for this module to have the type and
-- a smart constructor.
--
newtype Ordered f = Ordered {ordered :: f}
    deriving newtype (Eq, Show)

class MaybeOrdered f where
    maybeOrdered :: f -> Maybe (Ordered f)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance PartialOrd a => MaybeOrdered [a] where
    maybeOrdered = fmap Ordered . maybeOrderedList

instance PartialOrd a => MaybeOrdered (NonEmpty a) where
    maybeOrdered = mapMaybeOrdered NE.nonEmpty <=< maybeOrdered . F.toList

instance PartialOrd a => MaybeOrdered (a, a) where
    maybeOrdered (a1, a2) = maybeOrderedTuple [a1, a2]

instance PartialOrd a => MaybeOrdered (a, a, a) where
    maybeOrdered (a1, a2, a3) = maybeOrderedTuple [a1, a2, a3]

instance PartialOrd a => MaybeOrdered (a, a, a, a) where
    maybeOrdered (a1, a2, a3, a4) = maybeOrderedTuple [a1, a2, a3, a4]

--------------------------------------------------------------------------------
-- Conversion to tuples
--------------------------------------------------------------------------------

class AsList a t | t -> a where
    toList :: t -> [a]
    fromList :: [a] -> Maybe t

instance AsList a [a] where
    toList as = as
    fromList as = Just as

instance AsList a (a, a) where
    toList (a1, a2) = [a1, a2]
    fromList [a1, a2] = Just (a1, a2)
    fromList _ = Nothing

instance AsList a (a, a, a) where
    toList (a1, a2, a3) = [a1, a2, a3]
    fromList [a1, a2, a3] = Just (a1, a2, a3)
    fromList _ = Nothing

instance AsList a (a, a, a, a) where
    toList (a1, a2, a3, a4) = [a1, a2, a3, a4]
    fromList [a1, a2, a3, a4] = Just (a1, a2, a3, a4)
    fromList _ = Nothing

mapAsList :: AsList a t => ([a] -> [a]) -> t -> Maybe t
mapAsList f = fromList . f . toList

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs xs = case tailMay xs of
    Nothing -> []
    Just ys -> xs `zip` ys

isOrdered :: (Foldable f, PartialOrd a) => f a -> Bool
isOrdered = all (uncurry leq) . consecutivePairs . F.toList

mapMaybeOrdered :: (a -> Maybe b) -> Ordered a -> Maybe (Ordered b)
mapMaybeOrdered f = fmap Ordered . f . ordered

maybeOrderedList :: PartialOrd a => [a] -> Maybe [a]
maybeOrderedList unsorted
    | isOrdered unsorted = Just unsorted
    | isOrdered   sorted = Just   sorted
    | otherwise = Nothing
  where
    sorted = flip L.sortBy unsorted $ \a1 a2 -> if
        | a1 <  a2  -> LT
        | a1 == a2  -> EQ
        | otherwise -> GT

maybeOrderedTuple :: (AsList a t, PartialOrd a) => [a] -> Maybe (Ordered t)
maybeOrderedTuple = mapMaybeOrdered fromList <=< maybeOrdered
