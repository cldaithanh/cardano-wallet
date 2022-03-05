{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.CoinSelection.Internal.Types.Equipartition
    where

import Prelude

import Cardano.Numeric.Util
    ( equipartitionNatural )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Proxy
    ( Proxy )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Safe
    ( tailMay )
import Test.QuickCheck
    ( Arbitrary, property )
import Test.QuickCheck.Classes
    ( Laws (..) )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class Equipartition a where
    equipartition :: a -> NonEmpty void -> NonEmpty a
    equipartitionDistance :: a -> a -> Natural
    equipartitionOrdering :: a -> a -> Bool

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

equipartitionLaws
    :: forall a.
        ( Arbitrary a
        , Arbitrary (NonEmpty ())
        , Eq a
        , Equipartition a
        , Monoid a
        , Show a
        )
    => Proxy a
    -> Laws
equipartitionLaws _ = Laws "Equipartition"
    [ ( "Distance"
      , property (equipartitionLaw_distance @a @()))
    , ( "Length"
      , property (equipartitionLaw_length @a @()))
    , ( "Ordering"
      , property (equipartitionLaw_ordering @a @()))
    , ( "Sum"
      , property (equipartitionLaw_sum @a @()))
    ]

equipartitionLaw_distance
    :: Equipartition a => a -> NonEmpty void -> Bool
equipartitionLaw_distance a count =
    (\(r :| rs) -> F.all ((<= 1) . equipartitionDistance r) rs)
    (equipartition a count)

equipartitionLaw_length
    :: Equipartition a => a -> NonEmpty void -> Bool
equipartitionLaw_length a count =
    ((== length count) . length)
    (equipartition a count)

equipartitionLaw_ordering
    :: Equipartition a => a -> NonEmpty void -> Bool
equipartitionLaw_ordering a count =
    (all (uncurry equipartitionOrdering) . consecutivePairs)
    (equipartition a count)

equipartitionLaw_sum
    :: (Eq a, Equipartition a, Monoid a) => a -> NonEmpty void -> Bool
equipartitionLaw_sum a count =
    ((== a) . F.fold)
    (equipartition a count)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Equipartition Natural where
    equipartition = equipartitionNatural

    equipartitionDistance n1 n2
        | n1 >= n2  = n1 - n2
        | otherwise = n2 - n1

    equipartitionOrdering n1 n2 = n1 <= n2

instance Equipartition [a] where
    equipartition as count =
        NE.unfoldr makeChunk (chunkLengths, as)
      where
        chunkLengths :: NonEmpty Int
        chunkLengths = fromIntegral @Natural @Int <$>
            equipartition (fromIntegral @Int @Natural (length as)) count

        makeChunk :: (NonEmpty Int, [a]) -> ([a], Maybe (NonEmpty Int, [a]))
        makeChunk (c :| mcs, bs) = case NE.nonEmpty mcs of
            Just cs -> (prefix, Just (cs, suffix))
            Nothing -> (bs, Nothing)
          where
            (prefix, suffix) = L.splitAt c bs

    equipartitionDistance xs ys = equipartitionDistance
        (fromIntegral @Int @Natural $ length xs)
        (fromIntegral @Int @Natural $ length ys)

    equipartitionOrdering xs ys = length xs <= length ys

instance Ord a => Equipartition (Set a) where
    equipartition set count =
        Set.fromList <$> equipartition (Set.toList set) count

    equipartitionDistance xs ys = equipartitionDistance
        (fromIntegral @Int @Natural $ Set.size xs)
        (fromIntegral @Int @Natural $ Set.size ys)

    equipartitionOrdering xs ys = length xs <= length ys

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

consecutivePairs :: Foldable f => f a -> [(a, a)]
consecutivePairs as = inner (F.toList as)
  where
    inner xs = case tailMay xs of
        Nothing -> []
        Just ys -> xs `zip` ys
