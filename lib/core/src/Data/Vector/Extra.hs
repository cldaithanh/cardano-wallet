module Data.Vector.Extra
    ( dropHead
    , dropLast
    )
    where

import Prelude

import Data.Vector
    ( Vector )

import qualified Data.Vector as V

dropHead :: Int -> Vector a -> Vector a
dropHead i v = V.drop i v

dropLast :: Int -> Vector a -> Vector a
dropLast i v = V.take (length v - i) v
