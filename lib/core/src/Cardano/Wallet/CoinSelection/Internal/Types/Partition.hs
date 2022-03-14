{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.CoinSelection.Internal.Types.Partition
    where

import Prelude

import Cardano.Numeric.Util
    ( partitionNatural )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( mapMaybe )
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
    partition :: a -> NonEmpty a -> (a, NonEmpty a)

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

instance Partition Natural where
    partition n as = maybe (n, 0 <$ as) (0, ) (partitionNatural n as)

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
