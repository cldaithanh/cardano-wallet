{-# LANGUAGE FlexibleInstances #-}

module Algebra.SetLike
    where

import Data.Monoid
    ( Sum (..) )
import Numeric.Natural
    ( Natural )

import Prelude

-- https://en.wikipedia.org/wiki/Field_of_sets
-- lattices
-- https://en.wikipedia.org/wiki/Boolean_algebra_(structure)

-- Every Boolean algebra gives rise to a Boolean ring, and vice versa, with
-- ring multiplication corresponding to conjunction or meet ∧, and ring
-- addition to exclusive disjunction or symmetric difference (not disjunction
-- ∨). However, the theory of Boolean rings has an inherent asymmetry between
-- the two operators, while the axioms and theorems of Boolean algebra express
-- the symmetry of the theory described by the duality principle.[1]
--
-- In mathematics, a field of sets is a mathematical structure consisting of a
-- pair ( X , F ) {\displaystyle (X,{\mathcal {F}})} {\displaystyle
-- (X,{\mathcal {F}})} consisting of a set X {\displaystyle X} X and a family F
-- {\displaystyle {\mathcal {F}}} {\mathcal {F}} of subsets of X {\displaystyle
-- X} X called an algebra over X {\displaystyle X} X that contains the empty
-- set as an element, and is closed under the operations of taking complements
-- in X , {\displaystyle X,} X, finite unions, and finite intersections.


-- Lattice has meet and join
--   -- (min, max) or (gcd, lcm) for natural numbers
--   -- (intersection, union) for sets
--
-- PartialOrd already exists
--
-- Can we write Difference laws in terms of meet and join?
--

class SetLike a where
    difference :: a -> a -> a
    disjoint :: a -> a -> Bool
    intersection :: a -> a -> a
    union :: a -> a -> a
    symmetricDifference :: a -> a -> a

instance SetLike (Sum Natural) where
    difference a1 a2
        | a1 > a2 = a1 - a2
        | otherwise = 0
    symmetricDifference a1 a2
        | a1 > a2 = a1 - a2
        | otherwise = a2 - a1
    disjoint a1 a2
        | a1 == 0 = True
        | a2 == 0 = True
        | otherwise = False
    intersection a1 a2 = 2 * min a1 a2
    union = (+)
