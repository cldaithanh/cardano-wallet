{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Algebra.PartialOrd.Ordered
    ( Ordered
    , ordered
    ) where

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Control.Monad
    ( (<=<), guard )
import Data.Functor
    ( ($>) )
import Data.List.AsList
    ( AsList (..), asList )
import Safe
    ( tailMay )
import Test.QuickCheck
    ( Arbitrary (..), suchThatMap )

import Prelude

import qualified Data.List as L

newtype Ordered t = Ordered {ordered :: t}
    deriving newtype (Eq, Show)

instance (Arbitrary t, AsList t, Monoid (Item t), PartialOrd (Item t)) =>
    Arbitrary (Ordered t)
  where
    arbitrary = arbitrary `suchThatMap` buildOrdered

assertOrdered :: (AsList t, PartialOrd (Item t)) => t -> Maybe (Ordered t)
assertOrdered t = guard (isOrdered t) $> Ordered t

buildOrdered
    :: (AsList t, PartialOrd (Item t), Semigroup (Item t))
    => t
    -> Maybe (Ordered t)
buildOrdered = assertOrdered <=< asList (L.scanl1 (<>))

isOrdered :: (AsList t, PartialOrd (Item t)) => t -> Bool
isOrdered = all (uncurry leq) . consecutivePairs . toList
  where
    consecutivePairs :: [a] -> [(a, a)]
    consecutivePairs xs = case tailMay xs of
        Nothing -> []
        Just ys -> xs `zip` ys
