{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Algebra.PartialOrd.Operators
    where

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Prelude
    ( Bool, Eq (..), (&&) )

class PartialOrdOperators a where
    (< ) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    (> ) :: a -> a -> Bool

instance PartialOrd a => PartialOrdOperators a where
    a1 <  a2 = a1 `leq` a2 && a1 /= a2
    a1 <= a2 = a1 `leq` a2
    a1 >= a2 = a2 `leq` a1
    a1 >  a2 = a2 `leq` a1 && a1 /= a2
