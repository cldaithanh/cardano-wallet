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

newtype Ordered f = Ordered {ordered :: f}
    deriving newtype (Eq, Show)

instance (Arbitrary t, AsList a t, Monoid a, PartialOrd a) =>
    Arbitrary (Ordered t)
  where
    arbitrary = arbitrary `suchThatMap` buildOrdered

assertOrdered :: (AsList a t, PartialOrd a) => t -> Maybe (Ordered t)
assertOrdered t
    | isOrdered (toList t) = Just (Ordered t)
    | otherwise = Nothing

buildOrdered :: (AsList a t, Monoid a, PartialOrd a) => t -> Maybe (Ordered t)
buildOrdered = assertOrdered <=< asList (L.scanl1 (<>))

isOrdered :: (AsList a t, PartialOrd a) => t -> Bool
isOrdered = all (uncurry leq) . consecutivePairs . toList
  where
    consecutivePairs :: [a] -> [(a, a)]
    consecutivePairs xs = case tailMay xs of
        Nothing -> []
        Just ys -> xs `zip` ys
