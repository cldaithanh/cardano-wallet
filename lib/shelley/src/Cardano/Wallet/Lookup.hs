-- | This module provides a set of small, easily unit-tested functions that are
-- designed to be composed to provide some of the logic involved with looking up
-- the wallet-managed private keys associated with data in a transaction. For
-- example, we might look at the the address of some TxIns, and see if the
-- wallet manages a private key that can witness to that address.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Lookup where

import Prelude

import Cardano.Api
    ( IsShelleyBasedEra, KeyWitness, Tx, TxBody (..), TxIn (..), TxIx (..) )
import Cardano.Wallet.Sign
import Data.Function
    ( (&) )
import Data.Maybe

import Cardano.Crypto.Wallet
    ( XPrv )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
    ( toShelleyTxId )

import Cardano.Wallet
    ( ErrWitnessTx (..)
    , HasDBLayer
    , readRewardAccount
    , readWallet
    , withRootKey
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , Passphrase (Passphrase)
    , WalletKey
    , getRawKey
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOwned, isOwned )
import Cardano.Wallet.Primitive.Model
    ( Wallet
    , applyBlocks
    , availableUTxO
    , currentTip
    , getState
    , initWallet
    , totalUTxO
    , updateState
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
import Cardano.Wallet.Shelley.Compatibility
    ( NetworkId )
import qualified Cardano.Wallet.Shelley.Compatibility as W
import Cardano.Wallet.Shelley.Transaction
    ( TxWitnessTag (..)
    , TxWitnessTagFor
    , WalletStyle (..)
    , mkByronWitness
    , mkShelleyWitness
    , txWitnessTagFor
    )
import Control.Monad.Except
    ( ExceptT, liftIO, withExceptT )
import qualified Data.Map.Strict as M

signTransaction
    :: Monad f
    => Tx era
    -> (TxBody era -> f [KeyWitness era])
    -> f (Tx era)
signTransaction tx lookupWits = do
    ws <- lookupWits (Cardano.getTxBody tx)
    pure $
        fromSigned tx
        & addWitnesses ws
        & toSigned

-- | Uses the wallet's address discovery state and UTxO set to produce a pair of
-- lookup functions that can convert a 'TxIn' to an address key that can be used
-- to spend that inupt.
withKeyStore
    :: forall ctx s k a.
        ( HasDBLayer IO s k ctx
        , IsOwned s k
        )
    => ctx
    -> WalletId
    -> Passphrase "raw"
    -> ((W.Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption")) -> IO a)
    -> ExceptT ErrWitnessTx IO a
withKeyStore ctx wid pwd action = do
    (adState, utxo) <- withExceptT ErrWitnessTxNoSuchWallet $ do
        (cp, _, pending) <- readWallet @ctx @s @k ctx wid
        pure (getState cp, totalUTxO @s pending cp)

    withRootKey @_ @s ctx wid pwd ErrWitnessTxWithRootKey $ \xprv scheme -> do
        let pwdP = preparePassphrase scheme pwd
            keyFrom :: W.Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption")
            keyFrom = isOwned adState (xprv, pwdP)
        liftIO $ action keyFrom

-- We are looking to define a set of functions that have the type:
--   f :: TxBody era -> f [KeyWitness era]
-- This is the core type of any lookup function. However, to make things easier,
-- we can define the following function to convert from "XPrv" to
-- "KeyWitness era".
makeWitnessFromXPrv
    :: forall k era
     . ( IsShelleyBasedEra era
       , WalletKey k
       , TxWitnessTagFor k
       )
    => NetworkId
    -> (k 'AddressK XPrv, Passphrase "encryption")
    -> W.Address
    -> TxBody era
    -> KeyWitness era
makeWitnessFromXPrv networkId (k, pwd) addr body =
    let
        sk = (getRawKey k, pwd)
    in
        case txWitnessTagFor @k of
            TxWitnessShelleyUTxO ->
                mkShelleyWitness body sk
            TxWitnessByronUTxO Icarus ->
                mkByronWitness body networkId Nothing sk
            TxWitnessByronUTxO Byron ->
                mkByronWitness body networkId (Just addr) sk

handler
    :: forall ctx s k a era.
        ( HasDBLayer IO s k ctx
        , IsOwned s k
        )
    => ctx
    -> WalletId
    -> Passphrase "raw"
    -> NetworkId
    -> TxBody era
    -> ExceptT ErrWitnessTx IO [KeyWitness era]
handler ctx wid pwd networkId txBody = do
    (adState, utxo) <- withExceptT ErrWitnessTxNoSuchWallet $ do
        (cp, _, pending) <- readWallet @ctx @s @k ctx wid
        pure (getState cp, totalUTxO @s pending cp)

    keyFrom <- withRootKey @_ @s ctx wid pwd ErrWitnessTxWithRootKey $ \xprv scheme -> do
        let pwdP = preparePassphrase scheme pwd
            keyFrom :: W.Address -> Maybe WitnessData
            keyFrom addr = do
                (key :: k 'AddressK XPrv , keyPwd :: Passphrase "encryption")
                    <- isOwned adState (xprv, pwdP) addr
                pure $ mkWitnessData addr key keyPwd
        pure keyFrom

    txIns <- lookupTxIns utxo (pure . keyFrom) txBody
    pure $ fmap (toKeyWitness networkId txBody) txIns

data WitnessData

mkWitnessData
    :: W.Address
    -> k 'AddressK XPrv
    -> Passphrase "encryption"
    -> WitnessData
mkWitnessData = undefined

toKeyWitness :: NetworkId -> TxBody era -> WitnessData -> KeyWitness era
toKeyWitness = undefined

-- x :: Applicative f => W.UTxO -> (W.Address -> f (Maybe XPrv)) -> TxBody era -> f [KeyWitness era]
-- x u f txBody =
--     fmap (\(addr, xprv) -> makeWitnessFromXPrv addr xprv txBody)
--     <$> lookupTxIns u f txBody

-- Now we can simply define a set of functions with the type:
--   f :: TxBody era -> f [XPrv]

-- Returns the set of private keys that can be used to witness the TxIns. That
-- is, the TxIn:
--   - has an output address found in the given UTxO
--   - whose output address returns a private key from the given wallet lookup
--     function
--
-- Important properties of this function are:
--
-- lookup/txins/empty-utxo:
--   ∀f txBody. lookupTxIns mempty f txBody = mempty
-- lookup/txins/empty-txins:
--   ∀f utxo. lookupTxIns utxo f (txBody { txIns = mempty }) = mempty
-- lookup/txins/lookup-fails:
--   ∀utxo txBody. lookupTxIns utxo (const Nothing) txBody = mempty
lookupTxIns
    :: Applicative f
    => W.UTxO
    -- ^ The UTxO of the wallet, we will use this to determine if we "know
    -- about" a TxIn
    -> (W.Address -> f (Maybe key))
    -- ^ A way of "looking up" the wallet-managed key for an address
    -> TxBody era
    -- ^ The transaction body to query
    -> f [key]
    -- ^ The list of private keys the wallet manages that are associated with
    -- this transaction body.
lookupTxIns utxo lookupFn =
    lookupAddressXPrv lookupFn . lookupTxInAddresses utxo

-- Finds TxIns in the TxBody that are also in the UTxO, and provides the output
-- address for those TxIns.
--
-- TODO: Coverage of Byron Tx's generated
-- lookup/txins/empty-utxo:
--     ∀txBody. lookupTxInAddresses mempty txBody = mempty
-- lookup/txins/empty-txins:
--     ∀utxo. lookupTxInAddresses utxo txBody { txIns = mempty } = mempty
lookupTxInAddresses :: W.UTxO -> TxBody era -> [W.Address]
lookupTxInAddresses utxo (Cardano.TxBody txBodyContent) =
    let
        txIns :: [W.TxIn]
        txIns =
            [ W.TxIn (W.fromShelleyTxId $ Cardano.toShelleyTxId txid) (fromIntegral ix)
            | TxIn txid (TxIx ix) <- fst <$> Cardano.txIns txBodyContent ]
    in
        (\(W.TxOut addr _) -> addr)
        <$> mapMaybe (`M.lookup` (W.unUTxO utxo)) txIns

-- Applies some possibly effectful lookup function to a set of addresses.
lookupAddressXPrv
    :: Applicative f
    => (W.Address -> f (Maybe key))
    -> [W.Address]
    -> f [key]
lookupAddressXPrv f = fmap catMaybes . traverse f
