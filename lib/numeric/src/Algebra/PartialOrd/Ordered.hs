{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}

module Algebra.PartialOrd.Ordered
    ( MaybeOrdered (..)
    , Ordered
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
import Data.Maybe
    ( mapMaybe )
import Prelude hiding
    ( Ord (..) )
import Safe
    ( tailMay )
import Test.QuickCheck
    ( Arbitrary (..), suchThatMap )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

newtype Ordered f = Ordered {ordered :: f}
    deriving newtype (Eq, Show)

class MaybeOrdered f where
    maybeOrdered :: f -> Maybe (Ordered f)

instance (Arbitrary f, MaybeOrdered f) => Arbitrary (Ordered f) where
    arbitrary = arbitrary `suchThatMap` maybeOrdered
    shrink = mapMaybe maybeOrdered . shrink . ordered

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

class MaybeToTuple f t where
    maybeToTuple :: f -> Maybe t

instance MaybeToTuple [a] (a, a) where
    maybeToTuple [a1, a2] = Just (a1, a2)
    maybeToTuple _ = Nothing

instance MaybeToTuple [a] (a, a, a) where
    maybeToTuple [a1, a2, a3] = Just (a1, a2, a3)
    maybeToTuple _ = Nothing

instance MaybeToTuple [a] (a, a, a, a) where
    maybeToTuple [a1, a2, a3, a4] = Just (a1, a2, a3, a4)
    maybeToTuple _ = Nothing

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
maybeOrderedList as
    | isOrdered result = Just result
    | otherwise = Nothing
  where
    result = flip L.sortBy as $ \a1 a2 -> if
        | a1 <  a2  -> LT
        | a1 == a2  -> EQ
        | otherwise -> GT

maybeOrderedTuple
    :: (MaybeToTuple [a] t, PartialOrd a) => [a] -> Maybe (Ordered t)
maybeOrderedTuple = mapMaybeOrdered maybeToTuple <=< maybeOrdered
