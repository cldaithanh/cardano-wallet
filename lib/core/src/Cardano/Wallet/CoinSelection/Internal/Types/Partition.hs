{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.CoinSelection.Internal.Types.Partition
    where

import Prelude

import Cardano.Numeric.Util
    ( partitionNatural )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Proxy
    ( Proxy )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary, property )
import Test.QuickCheck.Classes
    ( Laws (..) )

import qualified Data.Foldable as F

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class Partition a where
    partition :: a -> NonEmpty a -> (a, NonEmpty a)

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

partitionLaws
    :: forall a.
        ( Arbitrary a
        , Arbitrary (NonEmpty a)
        , Eq a
        , Monoid a
        , Partition a
        , Show a
        )
    => Proxy a
    -> Laws
partitionLaws _ = Laws "Partition"
    [ ( "Length"
      , property (partitionLaw_length @a))
    , ( "Sum"
      , property (partitionLaw_sum @a))
    ]

partitionLaw_length :: Partition a => a -> NonEmpty a -> Bool
partitionLaw_length a as =
    ((== length as) . length . snd)
    (partition a as)

partitionLaw_sum :: (Eq a, Monoid a, Partition a) => a -> NonEmpty a -> Bool
partitionLaw_sum a as =
    (\(r, rs) -> r <> F.fold rs == a)
    (partition a as)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Partition Natural where
    partition n as = maybe (n, 0 <$ as) (0, ) (partitionNatural n as)
