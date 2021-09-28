
module Cardano.Wallet.Sign.Gen
    (
    -- * Generators
      genOperation
    , genSign

    -- * Helpers
    , printOperation
    , doOperation
    , operation
    , liftOperation
    ) where

import Prelude

import Cardano.Api
    ( CardanoEra, IsCardanoEra )
import Cardano.Api.Typed.Gen
    ( genTx, genWitness, genWitnesses )
import Cardano.Wallet.Sign
import Hedgehog
    ( Gen, Range, forAll )

import qualified Hedgehog.Gen as Gen

genOperation :: CardanoEra era -> Gen (SignOperation era)
genOperation era = Gen.choice
    [ pure $ operation "id" id
    , liftOperation "addWitness" $ addWitness <$> genWitness era
    , liftOperation "addWitnesses" $ addWitnesses <$> genWitnesses era
    ]

genSign :: IsCardanoEra era => CardanoEra era -> Gen (Sign era)
genSign era = do
    tx <- genTx era
    pure $ fromSigned tx

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

data SignOperation era = SignOperation (Sign era -> Sign era) String

printOperation :: SignOperation era -> String
printOperation (SignOperation _ s) = s

doOperation :: SignOperation era -> (Sign era -> Sign era)
doOperation (SignOperation f _) = f

operation :: String -> (Sign era -> Sign era) -> SignOperation era
operation s f = SignOperation f s

liftOperation :: Functor f => String -> f (Sign era -> Sign era) -> f (SignOperation era)
liftOperation s ff = operation s <$> ff

instance Show (SignOperation era) where
    show = ("SignOperation: " <>) . printOperation
