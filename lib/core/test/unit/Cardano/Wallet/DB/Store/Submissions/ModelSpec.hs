

module Cardano.Wallet.DB.Store.Submissions.ModelSpec ( spec, genDeltas ) where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Sqlite.Schema
    ( LocalTxSubmission (LocalTxSubmission) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Submissions.Model
    ( DeltaTxLocalSubmission (..)
    , TxLocalSubmissionHistory (..)
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( mockSealedTx )
import Control.Monad
    ( forM )
import Test.Hspec
    ( Spec )
import Test.QuickCheck
    ( Gen, arbitrary, elements, frequency, sized, vectorOf )

import qualified Data.Map.Strict as Map

spec :: Spec
spec = pure ()

genDeltas :: WalletId
    -- ^ wallet holding the submissions
    -> TxLocalSubmissionHistory
    -- ^ submitted ones
    -> Gen (DeltaTxLocalSubmission)
genDeltas wid (TxLocalSubmissionHistory  old) = frequency $
    [(1, sized $ \n -> do
        tids <- fmap TxId <$> vectorOf n arbitrary
        locals <- forM tids $ \txId'@(TxId txId) -> do
                let sealed = mockSealedTx . getHash $ txId
                slot <- arbitrary
                pure (txId', LocalTxSubmission txId' wid slot sealed)

        pure $ Expand . TxLocalSubmissionHistory $ Map.fromList locals)
    ] <>
    [(2, Prune <$> elements (Map.keys old) ) | not (null old)]

