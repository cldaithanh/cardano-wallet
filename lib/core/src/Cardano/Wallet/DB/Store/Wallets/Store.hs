
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Implementation of a store for 'TxWalletsHistory'

-}
module Cardano.Wallet.DB.Store.Wallets.Store
    ( mkStoreTxWalletsHistory
    , DeltaTxWalletsHistory(..)
    , mkStoreWalletsMeta
    , mkStoreMetaWithSubmissions ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (LocalTxSubmissionWalletId, TxMetaWalletId), TxMeta, LocalTxSubmission )
import Cardano.Wallet.DB.Store.Meta.Model
    ( TxMetaHistory, mkTxMetaHistory )
import Cardano.Wallet.DB.Store.Meta.Store
    ( mkStoreMetaTransactions )
import Cardano.Wallet.DB.Store.Submissions.Model
    ( TxLocalSubmissionHistory )
import Cardano.Wallet.DB.Store.Submissions.Store
    ( mkStoreSubmissions )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxHistory (..), TxHistoryF (..), mkTxHistory )
import Cardano.Wallet.DB.Store.Transactions.Store
    ( mkStoreTransactions )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..)
    , DeltaWalletsMetaWithSubmissions (..)
    , embedConstrainedSubmissions
    , walletsLinkedTransactions
    )
import Control.Applicative
    ( liftA2 )
import Control.Monad
    ( forM, forM_ )
import Control.Monad.Except
    ( ExceptT (ExceptT), runExceptT )
import Data.DBVar
    ( Store (..), embedStore', pairStores )
import Data.DeltaMap
    ( DeltaMap (..) )
import Data.Generics.Internal.VL
    ( view )
import Data.List
    ( nub )
import Data.Map.Strict
    ( Map )
import Database.Persist.Sql
    ( SqlPersistT, deleteWhere, entityVal, selectList, (==.) )

import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore
import qualified Cardano.Wallet.DB.Store.Transactions.Model as TxStore
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map

mkStoreMetaWithSubmissions :: W.WalletId
    -> Store (SqlPersistT IO) DeltaWalletsMetaWithSubmissions
mkStoreMetaWithSubmissions wid =
    embedStore' embedConstrainedSubmissions
    $ pairStores
        (mkStoreMetaTransactions wid)
        (mkStoreSubmissions wid)

-- | Store for 'WalletsMeta' of multiple different wallets.
mkStoreWalletsMeta :: Store
        (SqlPersistT IO)
        (DeltaMap W.WalletId DeltaWalletsMetaWithSubmissions)
mkStoreWalletsMeta =
    Store
    { loadS = load
    , writeS = write
    , updateS = update
    }
  where
    write reset = forM_ (Map.assocs reset) $ \(wid,(metas,subs)) -> do
        writeS (mkStoreMetaTransactions wid) metas
        writeS (mkStoreSubmissions wid) subs
    update :: Map W.WalletId (TxMetaHistory, TxLocalSubmissionHistory)
        -> DeltaMap W.WalletId DeltaWalletsMetaWithSubmissions
        -> SqlPersistT IO ()
    update _ (Insert wid (metas,subs)) = do
        writeS (mkStoreMetaTransactions wid) metas
        writeS (mkStoreSubmissions wid) subs
    update _ (Delete wid) = do
        deleteWhere [TxMetaWalletId ==. wid ]
        deleteWhere [LocalTxSubmissionWalletId ==. wid ]
    update _ (Adjust wid (ChangeMeta da)) = do
        updateS (mkStoreMetaTransactions wid) undefined da
    update _ (Adjust wid (ChangeSubmissions da)) = do
        updateS (mkStoreSubmissions wid) undefined da
    load = do
        wids <- nub . fmap (view #txMetaWalletId . entityVal)
            <$> selectList @TxMeta [] []
        subsWids <- nub . fmap (view #localTxSubmissionWalletId . entityVal)
            <$> selectList @LocalTxSubmission [] []
        runExceptT $ do
            metas <- fmap (Map.fromList . zip wids)
                $ forM wids
                $ ExceptT . loadS . mkStoreMetaTransactions
            subs <- fmap (Map.fromList . zip subsWids)
                $ forM subsWids
                $ ExceptT . loadS . mkStoreSubmissions
            pure
                $ Map.mergeWithKey
                    (\_ a b -> Just (a, b))
                    (fmap (, mempty))
                    (const mempty)
                    metas
                    subs

mkStoreTxWalletsHistory :: Store (SqlPersistT IO) DeltaTxWalletsHistory
mkStoreTxWalletsHistory =
    Store
    { loadS =
          liftA2 (,) <$> loadS mkStoreTransactions <*> loadS mkStoreWalletsMeta
    , writeS = \(txHistory,txMetaHistory) -> do
          writeS mkStoreTransactions txHistory
          writeS mkStoreWalletsMeta txMetaHistory
    , updateS = \(txh@(TxHistoryF mtxh),mtxmh) -> \case
          ExpandTxWalletsHistory wid cs -> do
              updateS mkStoreTransactions txh
                  $ TxStore.Append
                  $ mkTxHistory
                  $ fst <$> cs
              updateS mkStoreWalletsMeta mtxmh
                  $ Adjust wid
                  $ ChangeMeta
                  $ TxMetaStore.Expand
                  $ mkTxMetaHistory wid cs
          ChangeTxMetaWalletsHistory wid change
              -> updateS mkStoreWalletsMeta mtxmh
              $ Adjust wid
              change
          GarbageCollectTxWalletsHistory -> mapM_
              (updateS mkStoreTransactions txh . DeleteTx)
              $ Map.keys
              $ Map.withoutKeys mtxh
              $ walletsLinkedTransactions mtxmh
          RemoveWallet wid -> updateS mkStoreWalletsMeta mtxmh $ Delete wid
    }

