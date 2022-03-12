{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.CoinSelection.Internal.Types.Equipartition
    where

import Prelude

import Cardano.Numeric.Util
    ( equipartitionNatural )
import Control.Arrow
    ( (&&&) )
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
    ( Arbitrary, Property, Testable, checkCoverage, cover, property )
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

equipartitionLaw_length
    :: Equipartition a => a -> NonEmpty void -> Bool
equipartitionLaw_length a count =
    length (equipartition a count) == length count

equipartitionLaw_sum
    :: (Eq a, Equipartition a, Monoid a) => a -> NonEmpty void -> Bool
equipartitionLaw_sum a count =
    F.fold (equipartition a count) == a

equipartitionLaw_distanceConsecutive
    :: Equipartition a => a -> NonEmpty void -> Bool
equipartitionLaw_distanceConsecutive a count =
    all ((<= 1) . uncurry equipartitionDistance)
        (consecutivePairs (equipartition a count))

equipartitionLaw_distanceLimits
    :: Equipartition a => a -> NonEmpty void -> Bool
equipartitionLaw_distanceLimits a count =
    ((<= 1) . uncurry equipartitionDistance . (NE.head &&& NE.last))
    (equipartition a count)

equipartitionLaw_orderingConsecutive
    :: Equipartition a => a -> NonEmpty void -> Bool
equipartitionLaw_orderingConsecutive a count =
    all (uncurry equipartitionOrdering)
        (consecutivePairs (equipartition a count))

equipartitionLaw_orderingLimits
    :: Equipartition a => a -> NonEmpty void -> Bool
equipartitionLaw_orderingLimits a count =
    (uncurry equipartitionOrdering . (NE.head &&& NE.last))
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
-- Testing
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
    [ ( "Length"
      , makeProperty equipartitionLaw_length)
    , ( "Sum"
      , makeProperty equipartitionLaw_sum)
    , ( "Distance (Consecutive)"
      , makeProperty equipartitionLaw_distanceConsecutive)
    , ( "Distance (Limits)"
      , makeProperty equipartitionLaw_distanceLimits)
    , ( "Ordering (Consecutive)"
      , makeProperty equipartitionLaw_orderingConsecutive)
    , ( "Ordering (Limits)"
      , makeProperty equipartitionLaw_orderingLimits)
    ]
  where
    makeProperty :: (a -> NonEmpty () -> Bool) -> Property
    makeProperty = property . makePropertyInner

    makePropertyInner
        :: (a -> NonEmpty () -> Bool)
        -> (a -> NonEmpty () -> Property)
    makePropertyInner condition value count =
        checkCoverage $
        buildCoverage value count result $
        condition value count
      where
        result = equipartition value count

    buildCoverage
        :: Testable prop
        => a
        -> NonEmpty ()
        -> NonEmpty a
        -> prop
        -> Property
    buildCoverage value count result
        = cover 1
            (length count == 1)
            "length count == 1"
        . cover 10
            (length count /= 1)
            "length count /= 1"
        . cover 1
            (value == mempty)
            "value == mempty"
        . cover 10
            (value /= mempty)
            "value /= mempty"
        . cover 1
            (NE.head result == mempty)
            "NE.head result == mempty"
        . cover 10
            (NE.head result /= mempty)
            "NE.head result /= mempty"
        . cover 1
            (NE.last result == mempty)
            "NE.last result == mempty"
        . cover 10
            (NE.last result /= mempty)
            "NE.last result /= mempty"
        . cover 1
            (NE.head result == NE.last result)
            "NE.head result == NE.last result"
        . cover 10
            (NE.head result /= NE.last result)
            "NE.head result /= NE.last result"
        . cover 1
            (equipartitionDistance (NE.head result) (NE.last result) == 0)
            "equipartitionDistance (NE.head result) (NE.last result) == 0"
        . cover 10
            (equipartitionDistance (NE.head result) (NE.last result) /= 0)
            "equipartitionDistance (NE.head result) (NE.last result) /= 0"
        . cover 1
            (all (uncurry (/=)) (consecutivePairs result))
            "all (uncurry (/=)) (consecutivePairs result)"
        . cover 1
            (all (uncurry (==)) (consecutivePairs result))
            "all (uncurry (==)) (consecutivePairs result)"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

consecutivePairs :: Foldable f => f a -> [(a, a)]
consecutivePairs as = inner (F.toList as)
  where
    inner xs = case tailMay xs of
        Nothing -> []
        Just ys -> xs `zip` ys
