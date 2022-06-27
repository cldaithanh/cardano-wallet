{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Module containing extra 'Cardano.Api' functionality needed by the wallet.
module Cardano.Api.Extra
    (
    -- * Era conversions
      withShelleyBasedTx
    , inAnyCardanoEra
    , asAnyShelleyBasedEra

    -- * Computing minimum UTxO values
    , MinimumUTxO (..)
    ) where

import Prelude

import Cardano.Api
    ( CardanoEra (..)
    , InAnyCardanoEra (..)
    , InAnyShelleyBasedEra (..)
    , IsCardanoEra (cardanoEra)
    , IsShelleyBasedEra
    , ShelleyBasedEra (..)
    , Tx
    )
import Cardano.Api.Shelley
    ( ShelleyLedgerEra, fromLedgerPParams )
import Control.DeepSeq
    ( NFData (..) )
import Cardano.Ledger.Core
    ( PParams )
import Data.Function
    ( on )

--------------------------------------------------------------------------------
-- Era conversions
--------------------------------------------------------------------------------

-- | Apply an era-parameterized function to an existentially-wrapped
-- tx.
withShelleyBasedTx
    :: InAnyShelleyBasedEra Tx
    -> (forall era. IsShelleyBasedEra era => Tx era -> a)
    -> a
withShelleyBasedTx (InAnyShelleyBasedEra _era tx) f
    = f tx

-- | Helper function for more easily creating an existential
-- @InAnyCardanoEra Tx@.
inAnyCardanoEra :: IsCardanoEra era => Tx era -> InAnyCardanoEra Tx
inAnyCardanoEra = InAnyCardanoEra cardanoEra

-- | "Downcast" an existentially wrapped tx.
asAnyShelleyBasedEra
    :: InAnyCardanoEra a
    -> Maybe (InAnyShelleyBasedEra a)
asAnyShelleyBasedEra = \case
    InAnyCardanoEra ByronEra _ ->
        Nothing
    InAnyCardanoEra ShelleyEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraShelley a
    InAnyCardanoEra AllegraEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraAllegra a
    InAnyCardanoEra MaryEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraMary a
    InAnyCardanoEra AlonzoEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraAlonzo a
    InAnyCardanoEra BabbageEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraBabbage a

--------------------------------------------------------------------------------
-- Computing minimum UTxO values
--------------------------------------------------------------------------------

data MinimumUTxO where
    MinimumUTxONone
        :: MinimumUTxO
    MinimumUTxOForShelleyBasedEra
        :: ShelleyBasedEra era
        -> PParams (ShelleyLedgerEra era)
        -> MinimumUTxO

instance Eq MinimumUTxO where
    (==) = (==) `on` show

instance Show MinimumUTxO where
    show = \case
        MinimumUTxONone ->
            "MinimumUTxONone"
        MinimumUTxOForShelleyBasedEra era pp -> unwords
            [ "MinimumUTxOForShelleyBasedEra"
            , show era
            , show (fromLedgerPParams era pp)
            ]

instance NFData MinimumUTxO where
    rnf = \case
        MinimumUTxONone -> rnf ()
        MinimumUTxOForShelleyBasedEra !_ !_ -> rnf ()
