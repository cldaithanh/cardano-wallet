{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO
    , genUTxOLarge
    , genUTxOLargeN
    , selectUTxOEntries
    , shrinkUTxO
    ) where

import Prelude

import Cardano.Wallet.Primitive.Model
    ( applyTxToUTxO )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundlePartitionNonNull )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxIn, TxOut (..) )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( TxWithoutId (..)
    , genTxIn
    , genTxInLargeRange
    , genTxOut
    , shrinkTxIn
    , shrinkTxOut
    , txWithoutIdToTx
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Monad
    ( foldM, replicateM )
import Data.Bifunctor
    ( first )
import Data.Maybe
    ( listToMaybe )
import Test.QuickCheck
    ( Gen
    , ShrinkState (..)
    , choose
    , chooseInt
    , elements
    , shrinkList
    , sized
    , vectorOf
    )
import Test.QuickCheck.Extra
    ( selectMapEntries, shrinkInterleaved )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- UTxO sets generated according to the size parameter
--------------------------------------------------------------------------------

genUTxO :: Gen UTxO
genUTxO = sized $ \size -> do
    entryCount <- choose (0, size)
    UTxO . Map.fromList <$> replicateM entryCount genEntry

shrinkUTxO :: UTxO -> [UTxO]
shrinkUTxO
    = take 16
    . fmap (UTxO . Map.fromList)
    . shrinkList shrinkEntry
    . Map.toList
    . unUTxO

genEntry :: Gen (TxIn, TxOut)
genEntry = (,) <$> genTxIn <*> genTxOut

shrinkEntry :: (TxIn, TxOut) -> [(TxIn, TxOut)]
shrinkEntry (i, o) = uncurry (,) <$> shrinkInterleaved
    (i, shrinkTxIn)
    (o, shrinkTxOut)

--------------------------------------------------------------------------------
-- Large UTxO sets
--------------------------------------------------------------------------------

genUTxOLarge :: Gen UTxO
genUTxOLarge = do
    entryCount <- choose (1024, 4096)
    genUTxOLargeN entryCount

genUTxOLargeN :: Int -> Gen UTxO
genUTxOLargeN entryCount = do
    UTxO . Map.fromList <$> replicateM entryCount genEntryLargeRange

genEntryLargeRange :: Gen (TxIn, TxOut)
genEntryLargeRange = (,)
    <$> genTxInLargeRange
    -- Note that we don't need to choose outputs from a large range, as inputs
    -- are already chosen from a large range:
    <*> genTxOut

--------------------------------------------------------------------------------
-- Selecting random UTxO entries
--------------------------------------------------------------------------------

-- | Selects up to a given number of entries at random from the given UTxO set.
--
-- Returns the selected entries and the remaining UTxO set with the entries
-- removed.
--
selectUTxOEntries :: UTxO -> Int -> Gen ([(TxIn, TxOut)], UTxO)
selectUTxOEntries = (fmap (fmap UTxO) .) . selectMapEntries . unUTxO
