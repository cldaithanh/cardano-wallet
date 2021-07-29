{-# LANGUAGE RecordWildCards #-}
-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Primitives for minting and burning tokens.

module Cardano.Wallet.Primitive.MintBurn
    ( -- * Types
      TxMintBurn
    , ToMint
    , ToBurn
    , toMint
    , toBurn
    , scripts
    , mkTxMintBurn
    , scriptWitnessesNeeded
    , scriptWitnessesProvided
    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash, Script )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId )
import Data.Set
    ( Set )

import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as TokenPolicy
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- TokenMap can only hold positive values, so we create wrapper types to
-- represent positive values (ToMint) and negative values (ToBurn).

-- | Tokens to mint.
type ToMint = TokenMap

-- | Tokens to burn.
type ToBurn = TokenMap

-- | Representation of the Cardano.API data for minting and burning. This is the
-- data necessary to submit to the Cardano.API backend.
data TxMintBurn = TxMintBurn
    { toMint  :: ToMint
    , toBurn  :: ToBurn
    , scripts :: [Script KeyHash]
    }
    deriving (Eq, Show)

data ErrTxMintBurn
    = ErrTxMintBurnMissingWitnesses (Set TokenPolicyId)
    | ErrTxMintBurnUneededWitnesses (Set TokenPolicyId)
    deriving (Eq, Show)

mkTxMintBurn
    :: ToMint
    -> ToBurn
    -> [Script KeyHash]
    -> Either ErrTxMintBurn TxMintBurn
mkTxMintBurn toMint toBurn scripts =
    let
        txMintBurn = TxMintBurn toMint toBurn scripts
        missing = scriptWitnessesMissing txMintBurn
        uneeded = scriptWitnessesUnneeded txMintBurn
    in
        case (Set.toList missing, Set.toList uneeded) of
            (_      , _x:_xs) -> Left $ ErrTxMintBurnUneededWitnesses uneeded
            (_x:_xs ,    _  ) -> Left $ ErrTxMintBurnMissingWitnesses missing
            ([]     ,   []  ) -> Right txMintBurn


-- | Get the set of token policy IDs for which a script witness has not been
-- provided.
scriptWitnessesMissing :: TxMintBurn -> Set TokenPolicyId
scriptWitnessesMissing txMintBurn =
    let
        witnessesNeeded :: Set TokenPolicyId
        witnessesProvided :: Set TokenPolicyId
        witnessesMissing :: Set TokenPolicyId

        witnessesNeeded = scriptWitnessesNeeded txMintBurn
        witnessesProvided = scriptWitnessesProvided txMintBurn

        witnessesMissing = witnessesNeeded Set.\\ witnessesProvided
    in
        witnessesMissing

-- | Get the token policy IDs of script witnesses which have been provided but
-- are not necessary.
scriptWitnessesUnneeded :: TxMintBurn -> Set TokenPolicyId
scriptWitnessesUnneeded txMintBurn =
    let
        witnessesNeeded :: Set TokenPolicyId
        witnessesProvided :: Set TokenPolicyId
        witnessesExtra :: Set TokenPolicyId

        witnessesNeeded = scriptWitnessesNeeded txMintBurn
        witnessesProvided = scriptWitnessesProvided txMintBurn

        witnessesExtra = witnessesProvided Set.\\ witnessesNeeded
    in
        witnessesExtra

-- | Get the set of token policy IDs which must be witnessed by a script
-- witness.
scriptWitnessesNeeded :: TxMintBurn -> Set TokenPolicyId
scriptWitnessesNeeded (TxMintBurn {..}) =
    -- The policy of any minted or burned asset must be witnessed
    Map.keysSet (TokenMap.toNestedMap toMint)
    `Set.union`
    Map.keysSet (TokenMap.toNestedMap toBurn)

-- | Get the set of script witnesses provided.
scriptWitnessesProvided :: TxMintBurn -> Set TokenPolicyId
scriptWitnessesProvided (TxMintBurn {..}) =
    Set.fromList
    $ TokenPolicy.tokenPolicyIdFromScript <$> scripts
