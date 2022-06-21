{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Util
    ( iterateN
    , unfoldN
    ) where

import Prelude

import Data.List.NonEmpty
    ( NonEmpty (..) )

import qualified Data.List.NonEmpty as NE

iterateN :: Monad m => Int -> (a -> m a) -> a -> m a
iterateN n next a0 =
    NE.last . (a0 :|) <$> unfoldN n next a0

unfoldN :: forall m a. Monad m => Int -> (a -> m a) -> a -> m [a]
unfoldN n next = fmap reverse . loop n []
  where
    loop :: Int -> [a] -> a -> m [a]
    loop !i !as !a0
        | i <= 0 = pure as
        | otherwise = do
            a1 <- next a0
            loop (i - 1) (a1 : as) a1
