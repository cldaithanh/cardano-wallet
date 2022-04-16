{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.Partition
    where

import Prelude

import Cardano.Numeric.Util
    ( partitionNatural )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( mapMaybe )
import Data.Monoid
    ( Sum (..) )
import Data.Proxy
    ( Proxy )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , Property
    , Testable
    , arbitrary
    , checkCoverage
    , forAllShrink
    , property
    , shrink
    )
import Test.QuickCheck.Classes
    ( Laws (..) )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class Partition a where

    partition
        :: a -> NonEmpty a -> (a, NonEmpty a)
    partitionMaybe
        :: a -> NonEmpty a -> Maybe (NonEmpty a)

    default partition
        :: Monoid a
        => a -> NonEmpty a -> (a, NonEmpty a)
    partition a as = case partitionMaybe a as of
        Nothing -> (a, mempty <$ as)
        Just bs -> (mempty, bs)

    default partitionMaybe
        :: (Eq a, Monoid a)
        => a -> NonEmpty a -> Maybe (NonEmpty a)
    partitionMaybe a as = case partition a as of
       (b, bs) | b == mempty -> Just bs
       _ -> Nothing

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

partitionLaw_length :: Partition a => a -> NonEmpty a -> Bool
partitionLaw_length a as =
    length (snd (partition a as)) == length as

partitionLaw_sum :: (Eq a, Monoid a, Partition a) => a -> NonEmpty a -> Bool
partitionLaw_sum a as =
    F.fold (uncurry NE.cons (partition a as)) == a

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Partition (Sum Natural) where
    partitionMaybe a as =
        fmap Sum <$> partitionNatural (getSum a) (getSum <$> as)

deriving via Sum Natural instance Partition Natural

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

partitionLaws
    :: forall a.
        ( Arbitrary a
        , Eq a
        , Monoid a
        , Partition a
        , Show a
        )
    => Proxy a
    -> Laws
partitionLaws _ = Laws "Partition"
    [ ( "Length"
      , makeProperty partitionLaw_length)
    , ( "Sum"
      , makeProperty partitionLaw_sum)
    ]
  where
    makeProperty :: (a -> NonEmpty a -> Bool) -> Property
    makeProperty =
        property . forAllShrink genWeights shrinkWeights . makePropertyInner
      where
        genWeights :: Gen (NonEmpty a)
        genWeights = (:|) <$> arbitrary <*> arbitrary

        shrinkWeights :: NonEmpty a -> [NonEmpty a]
        shrinkWeights = mapMaybe NE.nonEmpty . shrink . NE.toList

    makePropertyInner
        :: (a -> NonEmpty a -> Bool)
        -> NonEmpty a
        -> (a -> Property)
    makePropertyInner condition weights value =
        checkCoverage $
        buildCoverage value weights result $
        condition value weights
      where
        result = partition value weights

    buildCoverage
        :: Testable prop
        => a
        -> NonEmpty a
        -> (a, NonEmpty a)
        -> prop
        -> Property
    buildCoverage _value _weights _result = property
