{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Test.DBVar
    ( genUpdates
    , prop_StoreUpdates
    , GenDelta
    , Updates (..)
    , prop_StoreUpdatesWithState) where

import Prelude

import Control.Exception.Safe
    ( impureThrow )
import Control.Monad
    ( forM_ )
import Data.DBVar
    ( Store (loadS, updateS, writeS) )
import Data.Delta
    ( Delta (..) )
import Fmt
    ( Buildable, listF, pretty )
import Test.QuickCheck
    ( Blind (Blind), Gen, counterexample, sized )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monitor, pick )


-- | Given a value, generate a random delta starting from this value.
type GenDelta da = Base da -> Gen da

-- | A sequence of updates and values after updating.
-- The update that is applied *last* appears in the list *first*.
newtype Updates da = Updates [(Base da, da)]

instance Show da => Show (Updates da) where
    show (Updates xs) = show . map snd $ xs

-- | Randomly generate a sequence of updates
genUpdates :: Delta da => Gen (Base da) -> GenDelta da -> Gen (Updates da)
genUpdates gen0 more = sized $ \n -> go n [] =<< gen0
  where
    go 0 das _  = pure $ Updates das
    go n das a0 = do
        da <- more a0
        let a1 = apply da a0
        go (n-1) ((a1,da):das) a1

-- | Test whether 'updateS' and 'loadS' behave as expected.
--
-- TODO: Shrinking of the update sequence.
prop_StoreUpdates
    :: ( Monad m, Delta da, Eq (Base da), Buildable da, Show (Base da))
    => (forall b. m b -> PropertyM IO b)
    -- ^ Function to embed the monad in 'IO'
    -> Store m da
    -- ^ Store that is to be tested.
    -> Gen (Base da)
    -- ^ Generator for the initial value.
    -> GenDelta da
    -- ^ Generator for deltas.
    -> PropertyM IO ()
prop_StoreUpdates toPropertyM store gen0 more = do

    -- randomly generate a sequence of updates
    Blind a0 <- pick $ Blind <$> gen0
    Blind (Updates adas) <- pick $ Blind <$> genUpdates (pure a0) more
    let as  = map fst adas ++ [a0]
        das = map snd adas

    monitor $ counterexample $
        "\nUpdates applied:\n" <> pretty (listF das)

    -- apply those updates
    ea <- toPropertyM $ do
        writeS store a0
        -- first update is applied last!
        let updates = reverse $ zip das (drop 1 as)
        forM_ updates $ \(da,a) -> updateS store a da
        loadS store

    -- check whether the last value is correct
    case ea of
        Left err -> impureThrow err
        Right a  -> do
            monitor $ counterexample $ "\nExpected:\n" <> show (head as)
            monitor $ counterexample $ "\nGot:\n" <> show a
            assert $ a == head as


type ThreadState s x = s -> Gen (s, x)
-- | Given a value, generate a random delta starting from this value.
type GenDeltaT s da = Base da -> ThreadState s da




-- | Randomly generate a sequence of updates
genUpdateWithState :: Delta da
    => Gen (Base da)
    -> GenDeltaT s da
    -> ThreadState s (Updates da)
genUpdateWithState gen0 more s0 = sized $ \n -> go s0 n [] =<< gen0
  where
    go s 0 das _ = pure (s, Updates das)
    go s n das a0 = do
        (s',da) <- more a0 s
        let a1 = apply da a0
        go s' (n - 1) ((a1, da) : das) a1


-- | Test whether 'updateS' and 'loadS' behave as expected.
--
-- TODO: Shrinking of the update sequence.
prop_StoreUpdatesWithState
    :: ( Monad m, Delta da, Eq (Base da), Buildable da, Show (Base da))
    => (forall b. m b -> PropertyM IO b)
    -- ^ Function to embed the monad in 'IO'
    -> Store m da
    -- ^ Store that is to be tested.
    -> Gen (Base da)
    -- ^ Generator for the initial value.
    -> GenDeltaT s da
    -- ^ Generator for deltas.
    -> s 
    -- ^ context  to consume while generating 
    -> PropertyM IO s
prop_StoreUpdatesWithState toPropertyM store gen0 more s = do

    -- randomly generate a sequence of updates
    Blind a0 <- pick $ Blind <$> gen0
    Blind (s', Updates adas) <- pick $ Blind <$> genUpdateWithState (pure a0) more s
    let as  = map fst adas ++ [a0]
        das = map snd adas

    monitor $ counterexample $
        "\nUpdates applied:\n" <> pretty (listF das)

    -- apply those updates
    ea <- toPropertyM $ do
        writeS store a0
        -- first update is applied last!
        let updates = reverse $ zip das (drop 1 as)
        forM_ updates $ \(da,a) -> updateS store a da
        loadS store

    -- check whether the last value is correct
    case ea of
        Left err -> impureThrow err
        Right a  -> do
            monitor $ counterexample $ "\nExpected:\n" <> show (head as)
            monitor $ counterexample $ "\nGot:\n" <> show a
            assert $ a == head as
    pure s'