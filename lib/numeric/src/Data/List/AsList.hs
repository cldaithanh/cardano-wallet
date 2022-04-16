{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.List.AsList
    ( AsList (..)
    ) where

import Data.List.NonEmpty
    ( NonEmpty )

import Prelude

import qualified Data.List.NonEmpty as NE

class AsList t where
    type Item t
    toList :: t -> [Item t]
    fromList :: [Item t] -> Maybe t
    asList :: ([Item t] -> [Item t]) -> t -> Maybe t
    asList f = fromList . f . toList

instance AsList (NonEmpty a) where
    type Item (NonEmpty a) = a
    toList = NE.toList
    fromList = NE.nonEmpty

instance AsList [a] where
    type Item [a] = a
    toList as = as
    fromList as = Just as

instance AsList (a, a) where
    type Item (a, a) = a
    toList (a1, a2) = [a1, a2]
    fromList [a1, a2] = Just (a1, a2)
    fromList _ = Nothing

instance AsList (a, a, a) where
    type Item (a, a, a) = a
    toList (a1, a2, a3) = [a1, a2, a3]
    fromList [a1, a2, a3] = Just (a1, a2, a3)
    fromList _ = Nothing

instance AsList (a, a, a, a) where
    type Item (a, a, a, a) = a
    toList (a1, a2, a3, a4) = [a1, a2, a3, a4]
    fromList [a1, a2, a3, a4] = Just (a1, a2, a3, a4)
    fromList _ = Nothing
