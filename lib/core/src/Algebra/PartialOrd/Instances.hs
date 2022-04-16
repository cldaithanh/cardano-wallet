{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Algebra.PartialOrd.Instances
    where

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Data.Monoid
    ( Sum (..) )
import Numeric.Natural
    ( Natural )
import Prelude

instance PartialOrd Natural where
    leq = (<=)

deriving instance PartialOrd a => PartialOrd (Sum a)
