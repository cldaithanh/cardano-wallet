module Cardano.Wallet.Primitive.Types.TxSeqSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress )
import Cardano.Wallet.Primitive.Types.TxSeq.Gen
    ( genTxSeq )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO )
import Data.Function
    ( (&) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, forAll, property )

import qualified Cardano.Wallet.Primitive.Types.TxSeq as TxSeq

spec :: Spec
spec = do

    describe "genTxSeq" $ do
        it "prop_genTxSeq_isValid" $
            prop_genTxSeq_isValid & property

    describe "dropHeadTxs " $ do
        it "prop_dropHeadTxs_isValid" $
            prop_dropHeadTxs_isValid & property

    describe "dropLastTxs " $ do
        it "prop_dropLastTxs_isValid" $
            prop_dropLastTxs_isValid & property

prop_genTxSeq_isValid :: Property
prop_genTxSeq_isValid =
    forAll (genTxSeq genUTxO genAddress) TxSeq.isValid

prop_dropHeadTxs_isValid :: Property
prop_dropHeadTxs_isValid =
    forAll (genTxSeq genUTxO genAddress) $ \txs ->
        all TxSeq.isValid (TxSeq.dropHeadTxs txs)

prop_dropLastTxs_isValid :: Property
prop_dropLastTxs_isValid =
    forAll (genTxSeq genUTxO genAddress) $ \txs ->
        all TxSeq.isValid (TxSeq.dropLastTxs txs)
