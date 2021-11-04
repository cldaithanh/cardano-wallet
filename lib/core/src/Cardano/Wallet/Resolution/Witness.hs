{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Resolution.Witness where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Wallet.Primitive.Types.Tx as Wallet

-- These are about applying functions correctly (find all the correct txins,
-- apply function to each).
--
-- Simply applies generic key witness selection strategies to the Tx.

resolveTxInWitnesses
    :: (Wallet.TxIn -> [Cardano.KeyWitness era])
    -> (Cardano.Tx era -> [Cardano.KeyWitness era])
resolveTxInWitnesses resolveWits tx =
    let
        body@(Cardano.TxBody bodyContent) = Cardano.getTxBody tx

        txIns :: [Wallet.TxIn]
        txIns = [ fromCardanoTxIn i | (i, _) <- Cardano.txIns bodyContent ]
    in
        foldMap (resolveWits body) txIns

resolveTxInWitnessesSimplified
    :: (TxIn -> Maybe Address)
    -> (Address -> [Cardano.KeyWitness era])
    -> (TxIn -> [Cardano.KeyWitness era])
resolveTxInWitnessesSimplified resolveInput resolveAddress txIn =
    foldMap resolveAddress [ addr | (Just addr) <- resolveInput txIn ]

resolveTxWithdrawalWitnesses
    :: (TxBody era -> Wallet.RewardAccount -> [Cardano.KeyWitness era])
    -> (Cardano.Tx era -> [Cardano.KeyWitness era])
resolveTxWithdrawalWitnesses resolveWits tx =
    let
        body@(Cardano.TxBody bodyContent) = Cardano.getTxBody tx

        wdrls :: [Wallet.RewardAccount]
        wdrls =
            [ addr
            | (addr, _) <- fromCardanoWdrls $ Cardano.txWithdrawals bodyContent
            ]
    in
        foldMap (resolveWits body) wdrls

resolveTxExtraKeyWitnesses
    :: (Cardano.TxBody era -> Cardano.Hash Cardano.PaymentKey -> [Cardano.KeyWitness era])
    -> (Cardano.Tx era -> [Cardano.KeyWitness era])
resolveTxExtraKeyWitnesses resolveWits tx =
    let
        body@(Cardano.TxBody bodyContent) = Cardano.getTxBody tx

        extraKeys =
            case Cardano.txExtraKeyWits bodyContent of
                Cardano.TxExtraKeyWitnessesNone ->
                    []
                Cardano.TxExtraKeyWitnesses _ xs ->
                    xs
    in
        foldMap (resolveWits body) extraKeys

resolveTxInCollateralWitnesses
    :: (Wallet.TxIn -> [Cardano.KeyWitness era])
    -> (Cardano.Tx era -> [Cardano.KeyWitness era])
resolveTxInCollateralWitnesses resolveWits tx =
    let
        body@(Cardano.TxBody bodyContent) = Cardano.getTxBody tx

        collaterals =
            case Cardano.txInsCollateral bodyContent of
                Cardano.TxInsCollateralNone ->
                    []
                Cardano.TxInsCollateral _ is ->
                    fromCardanoTxIn <$> is
    in
        foldMap (resolveWits body) collaterals

-- These are about creating functions correctly. Wallet-specific,
-- wallet-provided strategies for selecting key witnesses from various data.

defaultWalletStrategy
    :: _rootk
    -> _scheme
    -> _pwd
    -> _cp
    -> NetworkId
    -> (XPrv, Passphrase "encryption")
    -> (Cardano.Tx era -> [Cardano.KeyWitness era])
defaultWalletStrategy rootK scheme pwd cp networkId stakeCreds =
    let
        t = resolveTxInWitnessesSimplified
                (txInResolver cp)
                (addressResolver rootK scheme pwd cp networkId)
    sconcat
        [ resolveTxInWitnesses t
        , resolveTxInCollateralWitnesses t
        , resolveTxWithdrawalWitnesses
            (rewardAcctResolver stakeCreds)
        , resolveTxExtraKeyWitnesses
            (withWalletResolveExtraKeyWitnesses rootK scheme pwd cp networkId)
        ]

addressResolver
    :: _rootk
    -> _scheme
    -> _pwd
    -> _cp
    -> NetworkId
    -> (TxBody era -> Address -> [Cardano.KeyWitness era])
addressResolver rootK scheme pwd cp networkId body addr = maybeToList $ do
    let
        pwdP = preparePassphrase scheme pwd
        resolveAddress = isOwned (getState cp) (rootK, pwdP)

    (k, pwd) <- resolveAddress addr
    pure $ case (txWitnessTagFor @k) of
      TxWitnessShelleyUTxO ->
          mkShelleyWitness body (getRawKey k, pwd)
      TxWitnessByronUTxO{} ->
          mkByronWitness body networkId addr (getRawKey k, pwd)

txInResolver
    :: _cp
    -> (TxIn -> Maybe Address)
txInResolver cp txIn = do
    TxOut addr _ <- UTxO.lookup i (totalUTxO mempty cp)
    pure addr

rewardAcctResolver
    :: (XPrv, Passphrase "encryption")
    -- Reward account
    -> (TxBody era -> Wallet.RewardAccount -> [Cardano.KeyWitness era])
rewardAcctResolver stakeCreds body acct' = maybeToList $ do
    let acct = toRewardAccountRaw $ toXPub $ fst stakeCreds
    guard (acct == acct') $> mkShelleyWitness body stakeCreds

withWalletResolveExtraKeyWitnesses
    :: _rootk
    -> _scheme
    -> _pwd
    -> _cp
    -> NetworkId
    -> (TxBody era -> Cardano.Hash Cardano.PaymentKey -> [Cardano.KeyWitness era])
withWalletResolveExtraKeyWitnesses rootK scheme pwd cp networkId body vkh =
    -- NOTE: We cannot resolve key hashes directly, so create a one-time
    -- temporary address with that key hash which is fine to lookup via the
    -- address lookup provided above. It works _fine_ because the discovery
    -- of addresses is done properly based on the address constituants (i.e.
    -- the key hash) and not the overall address itself.
    let
        addr = Cardano.makeShelleyAddress networkId
            (Cardano.PaymentCredentialByKey vkh)
            Cardano.NoStakeAddress
    in
        addressResolver rootK
                        scheme
                        pwd
                        cp
                        networkId
                        body
                        (fromCardanoAddress addr)
