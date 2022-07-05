{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 Copyright: © 2018-2022 IOHK
 License: Apache-2.0

Low level meta transactions pure model. Meta transactions are encoded "as" expressed in DB
tables.

-}
module Cardano.Wallet.DB.Store.Wallets.Model where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( TxId )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory (..)
    , ManipulateTxMetaHistory
    , TxMetaHistory (..)
    , mkTxMetaHistory
    )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxHistory, TxHistoryF (..), mkTxHistory )
import Data.Delta
    ( Delta (..) )
import Data.DeltaMap
    ( DeltaMap (Adjust, Insert) )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Fmt
    ( Buildable, build )

import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore
import qualified Cardano.Wallet.DB.Store.Transactions.Model as TxStore
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Data.Foldable
    ( toList )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data DeltaTxWalletsHistory
    = ExpandTxWalletsHistory W.WalletId [(W.Tx, W.TxMeta)]
    | ChangeTxMetaWalletsHistory W.WalletId ManipulateTxMetaHistory
    | GarbageCollectTxWalletsHistory
    | RemoveWallet W.WalletId
    deriving ( Show, Eq )

instance Buildable DeltaTxWalletsHistory where
    build = build . show

type TxWalletsHistory = (TxHistory, Map W.WalletId TxMetaHistory)

instance Delta DeltaTxWalletsHistory where
    type Base DeltaTxWalletsHistory = TxWalletsHistory
    apply (ExpandTxWalletsHistory wid cs) (txh,mtxmh) =
        ( apply (TxStore.Append $ mkTxHistory $ fst <$> cs) txh
        , mtxmh & case Map.lookup wid mtxmh of
              Nothing -> apply @(DeltaMap _ DeltaTxMetaHistory)
                  $ Insert wid
                  $ mkTxMetaHistory wid cs
              Just _ -> apply @(DeltaMap _ DeltaTxMetaHistory)
                  $ Adjust wid
                  $ TxMetaStore.Expand
                  $ mkTxMetaHistory wid cs)
    apply (ChangeTxMetaWalletsHistory wid change) (txh, mtxmh) =
        (txh, Map.filter (not . null . relations)
            $ mtxmh & apply (Adjust wid $ Manipulate change))
    apply GarbageCollectTxWalletsHistory (TxHistoryF txh, mtxmh) =
        ( TxHistoryF $ Map.restrictKeys txh $ walletsLinkedTransactions mtxmh
        , mtxmh)
    apply (RemoveWallet wid) (TxHistoryF txh, mtxmh) =
        ( TxHistoryF txh, Map.delete wid mtxmh )

linkedTransactions :: TxMetaHistory -> Set TxId
linkedTransactions
    (TxMetaHistory m) = Map.keysSet m

walletsLinkedTransactions :: Map W.WalletId TxMetaHistory -> Set TxId
walletsLinkedTransactions = Set.unions . toList .  fmap linkedTransactions
