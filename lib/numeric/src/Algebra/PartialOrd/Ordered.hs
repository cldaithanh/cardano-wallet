{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Algebra.PartialOrd.Ordered
    (
    -- * Structures that are guaranteed to be ordered.
      Ordered
    , ordered

    -- * Asserting that structures are ordered.
    , MaybeOrdered
    , asOrdered
    , isOrdered

    -- * Building structures that are guaranteed to be ordered.
    , BuildOrdered
    , buildOrdered
    ) where

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Control.Monad
    ( guard, (<=<) )
import Data.Functor
    ( ($>) )
import Data.List.AsList
    ( AsList (..), asList )
import Data.Maybe
    ( mapMaybe )
import Safe
    ( tailMay )
import Test.QuickCheck
    ( Arbitrary (..), suchThatMap )

import Prelude

import qualified Data.List as L

--------------------------------------------------------------------------------
-- Structures that are guaranteed to be ordered.
--------------------------------------------------------------------------------

-- | A structure that is guaranteed to be ordered.
--
-- The following property should hold for a value 'v' of type 'Ordered':
--
-- >>> isOrdered (ordered v) == True
--
newtype Ordered t = Ordered {ordered :: t}
    deriving newtype (Eq, Show)

--------------------------------------------------------------------------------
-- Asserting that structures are ordered.
--------------------------------------------------------------------------------

type MaybeOrdered t = (AsList t, PartialOrd (Item t))

-- | Asserts that the given structure is ordered.
--
-- Returns an 'Ordered' value if and only if the structure is ordered.
--
asOrdered :: MaybeOrdered t => t -> Maybe (Ordered t)
asOrdered t = guard (isOrdered t) $> Ordered t

-- | Tests whether the given structure is ordered.
--
-- Returns 'True' if and only if the structure is ordered.
--
isOrdered :: MaybeOrdered t => t -> Bool
isOrdered = all (uncurry leq) . consecutivePairs . toList
  where
    consecutivePairs :: [a] -> [(a, a)]
    consecutivePairs xs = case tailMay xs of
        Nothing -> []
        Just ys -> xs `zip` ys

--------------------------------------------------------------------------------
-- Building structures that are guaranteed to be ordered.
--------------------------------------------------------------------------------

type BuildOrdered t = (AsList t, PartialOrd (Item t), Semigroup (Item t))

-- | Builds an ordered structure from a structure not guaranteed to be ordered.
--
-- Pre-condition:
--
-- For every pair of values 'a' and 'b' within the given structure, the
-- following properties should hold:
--
-- >>> a `leq` (a <> b)
-- >>> b `leq` (a <> b)
--
-- Returns 'Nothing' if the pre-condition is not met.
--
buildOrdered :: BuildOrdered t => t -> Maybe (Ordered t)
buildOrdered = asOrdered <=< asList (L.scanl1 (<>))

--------------------------------------------------------------------------------
-- Generation of arbitrary ordered structures.
--------------------------------------------------------------------------------

instance (Arbitrary t, BuildOrdered t) => Arbitrary (Ordered t) where
    arbitrary = arbitrary `suchThatMap` buildOrdered
    shrink = mapMaybe asOrdered . shrink . ordered
