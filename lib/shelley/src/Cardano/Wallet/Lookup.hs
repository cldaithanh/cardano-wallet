-- | This module provides a set of small, easily unit-tested functions that are
-- designed to be composed to provide some of the logic involved with looking up
-- the wallet-managed private keys associated with data in a transaction. For
-- example, we might look at the the address of some TxIns, and see if the
-- wallet manages a private key that can witness to that address.

module Cardano.Wallet.Lookup where

import Prelude

import Cardano.Api
    ( KeyWitness, TxBody (..), TxIn (..), TxIx (..) )
import Data.Maybe

import Cardano.Crypto.Wallet
    ( XPrv )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
    ( toShelleyTxId )

import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
import qualified Cardano.Wallet.Shelley.Compatibility as W
import qualified Data.Map.Strict as M

-- We are looking to define a set of functions that have the type:
--   f :: TxBody era -> f [KeyWitness era]
-- This is the core type of any lookup function. However, to make things easier,
-- we can define the following function to convert from "XPrv" to
-- "KeyWitness era".
makeWitnessFromXPrv :: XPrv -> TxBody era -> KeyWitness era
makeWitnessFromXPrv = undefined

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
    -> (W.Address -> f (Maybe XPrv))
    -- ^ A way of "looking up" the wallet-managed key for an address
    -> TxBody era
    -- ^ The transaction body to query
    -> f [XPrv]
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
    => (W.Address -> f (Maybe XPrv))
    -> [W.Address]
    -> f [XPrv]
lookupAddressXPrv f = fmap catMaybes . traverse f
