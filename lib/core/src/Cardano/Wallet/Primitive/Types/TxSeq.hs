{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.TxSeq
    ( TxSeq (..)
    , assetIds
    , dropHeadTx
    , dropHeadTxs
    , dropLastTx
    , dropLastTxs
    , empty
    , foldUTxO
    , fromUTxO
    , headUTxO
    , isValid
    , lastUTxO
    , length
    , mapAssetIds
    , mapTxIds
    , removeAssetId
    , removeAssets
    , shrinkAssetIds
    , shrinkTxIds
    , toTxGroups
    , toTxs
    , txIds
    , unfoldNM
    ) where

import Prelude hiding
    ( length, seq )

import Cardano.Wallet.Primitive.Model
    ( applyTxToUTxO )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxIn, txAssetIds, txMapAssetIds, txMapTxIds, txRemoveAssetId )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Data.Bifoldable
    ( Bifoldable (..) )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )
import Data.Set
    ( Set )

import qualified Cardano.Wallet.Primitive.Types.StateDeltaSeq as Seq
import qualified Cardano.Wallet.Primitive.Types.Tx as Tx
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype TxSeq = TxSeq {unTxSeq :: StateDeltaSeq UTxO (Maybe Tx)}
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Public interface
--------------------------------------------------------------------------------

nextUTxO :: UTxO -> Maybe Tx -> UTxO
nextUTxO u = maybe u (`applyTxToUTxO` u)

unfoldNM :: Monad m => Int -> (UTxO -> m (Maybe Tx)) -> UTxO -> m TxSeq
unfoldNM i nextTxM
    = fmap TxSeq
    . Seq.unfoldNM i nextTxM ((fmap . fmap $ pure) nextUTxO)

empty :: TxSeq
empty = fromUTxO mempty

fromUTxO :: UTxO -> TxSeq
fromUTxO = TxSeq . Seq.fromState

headUTxO :: TxSeq -> UTxO
headUTxO = Seq.headState . unTxSeq

lastUTxO :: TxSeq -> UTxO
lastUTxO = Seq.lastState . unTxSeq

foldUTxO :: TxSeq -> UTxO
foldUTxO = bifoldMap id (const mempty) . unTxSeq

toTxs :: TxSeq -> [Tx]
toTxs = mconcat . toTxGroups

toTxGroups :: TxSeq -> [[Tx]]
toTxGroups (TxSeq s) = catMaybes <$> L.groupBy f (F.toList s)
  where
    f Nothing _ = False
    f _ Nothing = False
    f _ _       = True

length :: TxSeq -> Int
length = F.length . unTxSeq

dropHeadTx :: TxSeq -> Maybe TxSeq
dropHeadTx = fmap TxSeq . Seq.dropHead . unTxSeq

dropHeadTxs :: TxSeq -> [TxSeq]
dropHeadTxs = fmap TxSeq . Seq.dropHeads . unTxSeq

dropLastTx :: TxSeq -> Maybe TxSeq
dropLastTx = fmap TxSeq . Seq.dropLast . unTxSeq

dropLastTxs :: TxSeq -> [TxSeq]
dropLastTxs = fmap TxSeq . Seq.dropLasts . unTxSeq

isValid :: TxSeq -> Bool
isValid = (Just True ==) . Seq.isValidM safeAppendTxM . unTxSeq

assetIds :: TxSeq -> Set AssetId
assetIds = bifoldMap UTxO.assetIds (maybe mempty txAssetIds) . unTxSeq

txIds :: TxSeq -> Set (Hash "Tx")
txIds = bifoldMap UTxO.txIds (maybe mempty (Set.singleton . txId)) . unTxSeq

mapAssetIds :: (AssetId -> AssetId) -> TxSeq -> TxSeq
mapAssetIds f =
    TxSeq . bimap (UTxO.mapAssetIds f) (fmap (txMapAssetIds f)) . unTxSeq

mapTxIds :: (Hash "Tx" -> Hash "Tx") -> TxSeq -> TxSeq
mapTxIds f =
    TxSeq . bimap (UTxO.mapTxIds f) (fmap (txMapTxIds f)) . unTxSeq

removeAssetId :: TxSeq -> AssetId -> TxSeq
removeAssetId (TxSeq s) a = TxSeq $
    bimap (`UTxO.removeAssetId` a) (fmap (`txRemoveAssetId` a)) s

removeAssets :: TxSeq -> TxSeq
removeAssets s0 = F.foldl' removeAssetId s0 (assetIds s0)

shrinkAssetIds :: TxSeq -> TxSeq
shrinkAssetIds s = mapAssetIds toSimpleAssetId s
  where
    toSimpleAssetId :: AssetId -> AssetId
    toSimpleAssetId = mapToFunction
        (head simpleAssetIds)
        (Map.fromList $ F.toList (assetIds s) `zip` simpleAssetIds)

shrinkTxIds :: TxSeq -> TxSeq
shrinkTxIds s = mapTxIds toSimpleTxId s
  where
    toSimpleTxId :: Hash "Tx" -> Hash "Tx"
    toSimpleTxId = mapToFunction
        (head simpleTxIds)
        (Map.fromList $ F.toList (txIds s) `zip` simpleTxIds)

--------------------------------------------------------------------------------
-- Domain-specific constants
--------------------------------------------------------------------------------

simpleAssetIds :: [AssetId]
simpleAssetIds
    = AssetId (UnsafeTokenPolicyId $ Hash mempty)
    . UnsafeTokenName
    . T.encodeUtf8
    . T.pack
    . show <$> [0 :: Integer ..]

simpleTxIds :: [Hash "Tx"]
simpleTxIds = Hash . T.encodeUtf8 . T.pack . show <$> [0 :: Integer ..]

--------------------------------------------------------------------------------
-- Domain-specific functions
--------------------------------------------------------------------------------

canApplyTxToUTxO :: Tx -> UTxO -> Bool
canApplyTxToUTxO tx u =  (&&)
    (all inputRefIsValid (tx & Tx.resolvedInputs))
    (all inputRefIsValid (tx & Tx.resolvedCollateralInputs))
  where
    inputRefIsValid :: (TxIn, Coin) -> Bool
    inputRefIsValid (ti, c) = case UTxO.lookup ti u of
        Nothing -> False
        Just to -> Tx.txOutCoin to == c

safeAppendTx :: MonadFail m => UTxO -> Tx -> m UTxO
safeAppendTx = flip safeApplyTxToUTxO

safeAppendTxM :: MonadFail m => UTxO -> Maybe Tx -> m UTxO
safeAppendTxM u = maybe (pure u) (safeAppendTx u)

safeApplyTxToUTxO :: MonadFail m => Tx -> UTxO -> m UTxO
safeApplyTxToUTxO tx u
    | tx `canApplyTxToUTxO` u =
        pure $ tx `applyTxToUTxO` u
    | otherwise = fail
        "cannot spend an input that does not refer to a known UTxO"

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

mapToFunction :: Ord k => v -> Map k v -> (k -> v)
mapToFunction = flip . Map.findWithDefault
