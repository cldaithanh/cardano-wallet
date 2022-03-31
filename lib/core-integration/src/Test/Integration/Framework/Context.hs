{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Integration.Framework.Context (
    Context (..),
    PoolGarbageCollectionEvent (..),
    TxDescription (..),
) where

import Prelude

import Cardano.CLI (
    Port (..),
 )
import Cardano.Wallet.Api.Types (
    ApiEra,
 )
import Cardano.Wallet.Primitive.Types (
    EpochNo,
    NetworkParameters,
    PoolRetirementCertificate,
 )
import Cardano.Wallet.Primitive.Types.Address (
    Address,
 )
import Cardano.Wallet.Primitive.Types.Coin (
    Coin (..),
 )
import Cardano.Wallet.Transaction (
    DelegationAction,
 )
import Data.ByteString (
    ByteString,
 )
import Data.IORef (
    IORef,
 )
import Data.Text (
    Text,
 )
import GHC.Generics (
    Generic,
 )
import Network.HTTP.Client (
    Manager,
 )
import Network.URI (
    URI,
 )
import Numeric.Natural (
    Natural,
 )
import Test.Integration.Faucet (
    Faucet,
 )

-- | Context for integration tests.
data Context = Context
    { -- | A cleanup action.
      _cleanup ::
        IO ()
    , -- | The underlying base URL and manager used by the wallet client.
      _manager ::
        ( URI
        , Manager
        )
    , -- | Server TCP port.
      _walletPort ::
        Port "wallet"
    , -- | Provides access to funded wallets.
      _faucet ::
        Faucet
    , -- | A function to inject rewards into some stake address.
      _moveRewardsToScript ::
        (ByteString, Coin) ->
        IO ()
    , -- | A fee estimator.
      _feeEstimator :: TxDescription -> (Natural, Natural)
    , -- | Blockchain parameters for the underlying chain.
      _networkParameters :: NetworkParameters
    , -- | The complete list of pool garbage collection events.
      -- Most recent events are stored at the head of the list.
      _poolGarbageCollectionEvents ::
        IORef [PoolGarbageCollectionEvent]
    , -- | The main era the tests are expected to run on. Allows tests to make
      -- era-specific assertions.
      _mainEra ::
        ApiEra
    , -- | Base URL of the mock smash server.
      _smashUrl :: Text
    , -- | TODO: Remove once we can unify cardano-wallet-core-integration and
      -- cardano-wallet:integration, or when the wallet supports minting.
      --
      -- Cannot be used by several tests at a time. (!)
      _mintSeaHorseAssets :: Int -> Int -> Coin -> [Address] -> IO ()
    }
    deriving (Generic)

{- | Records the parameters and return value of a single call to the
   'removeRetiredPools' operation of 'Pool.DB.DBLayer'.
-}
data PoolGarbageCollectionEvent = PoolGarbageCollectionEvent
    { -- | The epoch number parameter.
      poolGarbageCollectionEpochNo ::
        EpochNo
    , -- | The pools that were removed from the database.
      poolGarbageCollectionCertificates ::
        [PoolRetirementCertificate]
    }
    deriving (Eq, Show)

-- | Describe a transaction in terms of its inputs and outputs.
data TxDescription
    = DelegDescription DelegationAction
    | PaymentDescription
        { nInputs ::
            Int
        , nOutputs ::
            Int
        , nChanges ::
            Int
        }
    deriving (Show)
