module Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex
    , genUTxOIndexLarge
    , genUTxOIndexLargeN
    , shrinkUTxOIndex
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO, genUTxOLarge, genUTxOLargeN, shrinkUTxO )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Test.QuickCheck
    ( Gen, shrinkMapBy )

import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex

--------------------------------------------------------------------------------
-- Indices generated according to the size parameter
--------------------------------------------------------------------------------

genUTxOIndex :: Gen (UTxOIndex TxIn)
genUTxOIndex = UTxOIndex.fromMap . unUTxO <$> genUTxO

shrinkUTxOIndex :: UTxOIndex TxIn -> [UTxOIndex TxIn]
shrinkUTxOIndex = shrinkMapBy
    (UTxOIndex.fromMap . unUTxO)
    (UTxO . UTxOIndex.toMap)
    (shrinkUTxO)

--------------------------------------------------------------------------------
-- Large indices
--------------------------------------------------------------------------------

genUTxOIndexLarge :: Gen (UTxOIndex TxIn)
genUTxOIndexLarge = UTxOIndex.fromMap . unUTxO <$> genUTxOLarge

genUTxOIndexLargeN :: Int -> Gen (UTxOIndex TxIn)
genUTxOIndexLargeN n = UTxOIndex.fromMap . unUTxO <$> genUTxOLargeN n
