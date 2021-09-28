{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.SignSpec
    ( spec
    ) where

import Prelude

import Cardano.Api
    ( CardanoEra (..), Tx (..), getTxBody, getTxWitnesses )
import Data.Function
    ( (&) )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldReturn )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), Positive (..), Property, forAll, generate, property )
import Test.QuickCheck.Hedgehog
    ( hedgehog )

import Cardano.Api.Typed.Gen
import Cardano.Wallet.Sign
import Cardano.Wallet.Sign.Gen

import qualified Data.List as List
import qualified Data.Set as Set

spec :: Spec
spec = do
    parallel $ describe "Additive witness algebra" $ do
        it "sign/witnesses-preserved"
            prop_sign_witnessesPreserved
        it "sign/fromSigned/toSigned/no-op"
            prop_sign_noop
        it "sign/addWitness/always-adds"
            prop_sign_addWitness_alwaysAdds
        it "sign/addWitnesses/empty-is-noop"
            prop_sign_addWitnesses_emptyIsNoop
        it "sign/addWitnesses/addWitness"
            prop_sign_addWitnesses_addWitness
        it "sign/fromSigned/fromUnsigned"
            prop_sign_fromSigned_fromUnsigned
        it "sign/fromUnsigned/fromSigned"
            prop_sign_fromUnsigned_fromSigned
        it "sign/addWitnesses/increases-number-of-witnesses"
            prop_sign_addWitness_increases_number_of_witnesses
        it "sign/addWitnesses/adds-a-witness"
            prop_sign_addWitness_adds_a_witness
        it "sign/never-modifies-tx-body"
            prop_sign_never_modifies_tx_body

prop_sign_witnessesPreserved :: Property
prop_sign_witnessesPreserved = property $
    forAll (hedgehog $ genTx ShelleyEra) $ \tx ->
    forAll (hedgehog $ genOperation ShelleyEra) $ \signOp ->
        let
            op = doOperation signOp
        in
            getTxWitnesses tx
                `isSubsetOf`
                    getTxWitnesses (toSigned (op (fromSigned tx)))
    where
        isSubsetOf a b = all (`elem` b) a

prop_sign_noop :: Property
prop_sign_noop = property $
    forAll (hedgehog $ genTx ShelleyEra) $ \tx ->
        toSigned (fromSigned tx) == tx

prop_sign_addWitness_alwaysAdds :: Property
prop_sign_addWitness_alwaysAdds = property $
    forAll (hedgehog $ genSign ShelleyEra) $ \x ->
    forAll (hedgehog $ genWitness ShelleyEra) $ \w ->
        addWitness w x /= x

prop_sign_addWitnesses_emptyIsNoop :: Property
prop_sign_addWitnesses_emptyIsNoop = property $
    forAll (hedgehog $ genSign ShelleyEra) $ \x ->
        addWitnesses [] x == x

prop_sign_addWitnesses_addWitness :: Property
prop_sign_addWitnesses_addWitness = property $
    forAll (hedgehog $ genSign ShelleyEra) $ \x ->
    forAll (hedgehog $ genWitnesses ShelleyEra) $ \ws ->
        addWitnesses ws x
        == foldr (.) id (fmap addWitness ws) x

prop_sign_fromSigned_fromUnsigned :: Property
prop_sign_fromSigned_fromUnsigned = property $
    forAll (hedgehog $ genTx ShelleyEra) $ \tx ->
        fromSigned tx == (fromUnsigned (getTxBody tx)
                         & addWitnesses (getTxWitnesses tx))

prop_sign_fromUnsigned_fromSigned :: Property
prop_sign_fromUnsigned_fromSigned = property $
    forAll (hedgehog $ genTxBody ShelleyEra) $ \txBody ->
        fromUnsigned txBody == fromSigned (Tx txBody [])

prop_sign_addWitness_increases_number_of_witnesses :: Property
prop_sign_addWitness_increases_number_of_witnesses = property $
    forAll (hedgehog $ genSign ShelleyEra) $ \x ->
    forAll (hedgehog $ genWitness ShelleyEra) $ \w ->
        numWitnesses (addWitness w x) == numWitnesses x + 1
    where
        numWitnesses :: forall era. Sign era -> Int
        numWitnesses = length . getTxWitnesses . toSigned

prop_sign_addWitness_adds_a_witness :: Property
prop_sign_addWitness_adds_a_witness = property $
    forAll (hedgehog $ genSign ShelleyEra) $ \x ->
    forAll (hedgehog $ genWitness ShelleyEra) $ \w ->
        getWitnesses (addWitness w x) List.\\ getWitnesses x == [w]

    where
        getWitnesses = getTxWitnesses . toSigned

prop_sign_never_modifies_tx_body :: Property
prop_sign_never_modifies_tx_body = property $
    forAll (hedgehog $ genSign ShelleyEra) $ \x ->
    forAll (hedgehog $ genOperation ShelleyEra) $ \signOp ->
        let
            op = doOperation signOp
        in
            getTxBody (toSigned (op x)) == getTxBody (toSigned x)
