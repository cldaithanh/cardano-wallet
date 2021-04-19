{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module provides the main entry point for creating migration plans.
--
-- Use 'createPlan' to create a migration plan.
--
module Cardano.Wallet.Primitive.Migration
    (
    -- * Creating a migration plan
      createPlan

    -- * Types
    , MigrationPlan (..)
    , RewardWithdrawal (..)
    , Selection (..)
    , TxSize (..)

    ) where

import Prelude

import Cardano.Wallet.Primitive.Migration.Selection
    ( RewardWithdrawal (..), Selection (..), TxSize (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxConstraints (..), TxIn, TxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()

import qualified Cardano.Wallet.Primitive.Migration.Planning as Planning

-- | Represents a plan for how to migrate a 'UTxO' set.
--
-- See 'createPlan'.
--
data MigrationPlan s = MigrationPlan
    { selections :: ![Selection (TxIn, TxOut) s]
    , unselected :: !UTxO
    , totalFee :: !Coin
    }
    deriving (Eq, Show)

-- | Creates a migration plan for the given UTxO set and reward withdrawal
--   amount.
--
-- See 'MigrationPlan'.
--
createPlan
    :: TxSize s
    => TxConstraints s
    -> UTxO
    -> RewardWithdrawal
    -> MigrationPlan s
createPlan constraints utxo rewardWithdrawal = MigrationPlan
    { selections = view #selections plan
    , unselected = Planning.uncategorizeUTxO (view #unselected plan)
    , totalFee = view #totalFee plan
    }
  where
    categorizedUTxO = Planning.categorizeUTxO constraints utxo
    plan = Planning.createPlan constraints categorizedUTxO rewardWithdrawal
