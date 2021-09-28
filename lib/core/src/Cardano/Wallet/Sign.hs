-- | This module provides an interface for interacting with the additive signing
-- functions of cardano-wallet.
--
-- Its purpose is to provide a low-level algebra (it works directly on the
-- cardano-api types) that allows signatures to be added to an existing Tx,
-- while maintaining the following important properties:
--
-- 1. Under no circumstances will the algebra modify or destroy existing
-- witnesses of the Tx (sign/witnesses-preserved)
--     ∀tx (f :: Sign era -> Sign era)
--     . getTxWitnesses tx
--       ⊆ getTxWitnesses (toSigned (f (fromSigned tx)))
--
-- sign/fromSigned/toSigned/noop:
--     ∀tx. toSigned (fromSigned tx) = tx
-- sign/addWitness/alwaysAdds:
--     ∀ws x. addWitness w x ≠ x
-- sign/addWitnesses/empty-is-noop:
--     ∀ws x. addWitnesses [] x = x
-- sign/addWitnesses/addWitness:
--     ∀ws x. addWitnesses ws x = foldr (.) id (fmap addWitness ws) x
-- sign/fromSigned/fromUnsigned:
--     ∀tx. fromSigned tx = fromUnsigned (getTxBody tx)
--                          & addWitnesses (getTxWitnesses tx)
-- sign/fromUnsigned/fromSigned:
--     ∀txBody. fromUnsigned txBody = fromSigned $ Tx txBody []
-- sign/addWitness/increases-number-of-witnesses
--     ∀w x. length (getTxWitnesses (toSigned (addWitness w x)))
--           = length (getTxWitnesses (toSigned x)) + 1
-- sign/addWitness/adds-a-witness
--     ∀w x. getTxWitnesses (toSigned (addWitness w x))
--           `diff` getTxWitnesses (toSigned x)
--           = [w]
-- sign/never-modifies-tx-body
--     ∀f x. getTxBody (toSigned (f x)) = getTxBody (toSigned x)
module Cardano.Wallet.Sign
    (
    -- * Abstract algebra type
    Sign

    -- * Constructors
    , fromSigned
    , fromUnsigned

    -- * Operations
    , addWitness
    , addWitnesses

    -- * Denotation
    , toSigned
    ) where

import Prelude

import Cardano.Api
    ( KeyWitness, Tx (..), TxBody )

data Sign era = Sign (Tx era)

fromSigned :: Tx era -> Sign era
fromSigned = Sign

fromUnsigned :: TxBody era -> Sign era
fromUnsigned body = Sign $ Tx body []

addWitness :: KeyWitness era -> Sign era -> Sign era
addWitness w (Sign (Tx body ws)) = Sign $ Tx body (ws <> [w])

addWitnesses :: [KeyWitness era] -> Sign era -> Sign era
addWitnesses ws' (Sign (Tx body ws)) = Sign $ Tx body (ws <> ws')

toSigned :: Sign era -> Tx era
toSigned (Sign tx) = tx

instance Show (Sign era) where
    show = show . toSigned

-- TODO equality laws
instance Eq (Sign era) where
    a == b = toSigned a == toSigned b
