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
    ( MaybeOrdered (..)
    , Ordered
    , ordered
    ) where

import Algebra.PartialOrd.Ordered.Internal
    ( MaybeOrdered (..)
    , AsList (..)
    , Ordered
    , ordered
    , mapAsList
    )

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Algebra.PartialOrd.Operators
    ( PartialOrdOperators (..) )
import Control.Monad
    ( (>=>) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Maybe
    ( mapMaybe )
import Prelude hiding
    ( Ord (..) )
import Safe
    ( tailMay )
import Test.QuickCheck
    ( Arbitrary (..), Gen (..), suchThatMap )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

buildOrdered :: Monoid a => [a] -> [a]
buildOrdered = L.scanl1 (<>)

instance (Arbitrary t, MaybeOrdered t, Monoid a, PartialOrd a, AsList a t)
    => Arbitrary (Ordered t)
  where
    arbitrary = arbitrary `suchThatMap`
        (mapAsList buildOrdered >=> maybeOrdered)
