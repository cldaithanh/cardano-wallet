{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.List.AsList
    ( AsList (..)
    ) where

import Data.List.NonEmpty
    ( NonEmpty )

import Prelude

import qualified Data.List.NonEmpty as NE

class AsList a t | t -> a where
    toList :: t -> [a]
    fromList :: [a] -> Maybe t
    asList :: ([a] -> [a]) -> t -> Maybe t
    asList f = fromList . f . toList

instance AsList a (NonEmpty a) where
    toList = NE.toList
    fromList = NE.nonEmpty

instance AsList a [a] where
    toList as = as
    fromList as = Just as

instance AsList a (a, a) where
    toList (a1, a2) = [a1, a2]
    fromList [a1, a2] = Just (a1, a2)
    fromList _ = Nothing

instance AsList a (a, a, a) where
    toList (a1, a2, a3) = [a1, a2, a3]
    fromList [a1, a2, a3] = Just (a1, a2, a3)
    fromList _ = Nothing

instance AsList a (a, a, a, a) where
    toList (a1, a2, a3, a4) = [a1, a2, a3, a4]
    fromList [a1, a2, a3, a4] = Just (a1, a2, a3, a4)
    fromList _ = Nothing
