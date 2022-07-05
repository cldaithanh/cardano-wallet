
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Data type 'TxLocalSubmissionHistory' for storing a set of submitted transactions.
Transactions are encoded "as" expressed in DB tables.

-}
module Cardano.Wallet.DB.Store.Submissions.Model
    ( TxLocalSubmissionHistory(..)
    , DeltaTxLocalSubmission(..) ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( LocalTxSubmission (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId )
import Data.Delta
    ( Delta(..) )
import Fmt
    ( Buildable(..) )
import GHC.Generics
    ( Generic )
import Data.Map.Strict
    ( Map )

import qualified Data.Map.Strict as Map

-- | all wallet local-submissions-transactions indexed by transaction id
newtype TxLocalSubmissionHistory =
    TxLocalSubmissionHistory {relations :: Map TxId LocalTxSubmission}
    deriving ( Eq, Show, Generic, Monoid, Semigroup )

data DeltaTxLocalSubmission
    = Expand TxLocalSubmissionHistory
    | Prune TxId
    -- ^ add or overwrite (by id) local-submission-transactions
    deriving ( Eq, Show, Generic )

instance Buildable DeltaTxLocalSubmission where
    build = build . show

instance Delta DeltaTxLocalSubmission where
    type Base DeltaTxLocalSubmission = TxLocalSubmissionHistory
    apply (Expand addendum) x = addendum <> x
    apply (Prune tid) (TxLocalSubmissionHistory m) = TxLocalSubmissionHistory
        $ Map.delete tid m
