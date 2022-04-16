{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Algebra.PartialOrd.Ordered
    ( Ordered
    , ordered
    ) where

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Control.Monad
    ( (<=<) )
import Data.List.AsList
    ( AsList (..) )
import Prelude hiding
    ( Ord (..) )
import Safe
    ( tailMay )
import Test.QuickCheck
    ( Arbitrary (..), suchThatMap )

import qualified Data.List as L

newtype Ordered t = Ordered {ordered :: t}
    deriving newtype (Eq, Show)

instance (Arbitrary t, AsList t, Monoid (Item t), PartialOrd (Item t)) =>
    Arbitrary (Ordered t)
  where
    arbitrary = arbitrary `suchThatMap` buildOrdered

assertOrdered :: (AsList t, PartialOrd (Item t)) => t -> Maybe (Ordered t)
assertOrdered t
    | isOrdered (toList t) = Just (Ordered t)
    | otherwise = Nothing

buildOrdered
    :: (AsList t, Monoid (Item t), PartialOrd (Item t))
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
