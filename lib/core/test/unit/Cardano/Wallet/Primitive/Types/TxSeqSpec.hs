{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.TxSeqSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress )
import Cardano.Wallet.Primitive.Types.TxSeq.Gen
    ( ShrinkableTxSeq, genTxSeq, shrinkTxSeq, unwrapTxSeq )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO )
import Data.Function
    ( (&) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Property, forAll, property, (===) )

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
    forAll (genTxSeq genUTxO genAddress) $ \(unwrapTxSeq -> txs) ->
        TxSeq.isValid txs

prop_dropHeadTxs_isValid :: ShrinkableTxSeq -> Property
prop_dropHeadTxs_isValid (unwrapTxSeq -> txs) =
    all TxSeq.isValid (TxSeq.dropHeadTxs txs) === True

prop_dropLastTxs_isValid :: ShrinkableTxSeq -> Property
prop_dropLastTxs_isValid (unwrapTxSeq -> txs) =
    all TxSeq.isValid (TxSeq.dropLastTxs txs) === True

instance Arbitrary ShrinkableTxSeq where
    arbitrary = genTxSeq genUTxO genAddress
    shrink = shrinkTxSeq
