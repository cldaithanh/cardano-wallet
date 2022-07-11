{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.BM.Plugin
    ( loadPlugin )
import Cardano.BM.Trace
    ( appendName )
import Cardano.CLI
    ( LogOutput (..)
    , Port (..)
    , ekgEnabled
    , getEKGURL
    , getPrometheusURL
    , withLogging
    )
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Startup
    ( installSignalHandlersNoLogging
    , setDefaultFilePermissions
    , withUtf8Encoding
    )
import Cardano.Wallet.Api.Types
    ( DecodeAddress (..), EncodeAddress (..) )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer, stdoutTextTracer, trMessageText )
import Cardano.Wallet.Network.Ports
    ( portFromURL )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..)
    , Tracers
    , serveWallet
    , setupTracers
    , tracerSeverities
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..) )
import Cardano.Wallet.Shelley.Faucet
    ( initFaucet )
import Cardano.Wallet.Shelley.Launch
    ( withSystemTempDir )
import Cardano.Wallet.Shelley.Launch.Cluster
    ( ClusterLog
    , Credential (..)
    , RunningNode (..)
    , clusterEraFromEnv
    , clusterEraToString
    , clusterToApiEra
    , localClusterConfigFromEnv
    , moveInstantaneousRewardsTo
    , oneMillionAda
    , sendFaucetAssetsTo
    , testLogDirFromEnv
    , testMinSeverityFromEnv
    , walletListenFromEnv
    , walletMinSeverityFromEnv
    , withCluster
    , withSMASH
    )
import Cardano.Wallet.TokenMetadata.MockServer
    ( queryServerStatic, withMetadataServer )
import Control.Arrow
    ( first )
import Control.Monad
    ( when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Data.Either.Combinators
    ( whenLeft )
import Data.IORef
    ( IORef, atomicModifyIORef', newIORef )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Network.URI
    ( URI )
import Ouroboros.Network.Client.Wallet
    ( tunedForMainnetPipeliningStrategy )
import System.Directory
    ( createDirectory )
import System.Environment
    ( setEnv )
import System.FilePath
    ( (</>) )
import Test.Hspec.Core.Spec
    ( Spec, SpecWith, describe, parallel, sequential )
import Test.Hspec.Extra
    ( aroundAll, hspecMain )
import Test.Integration.Faucet
    ( byronIntegrationTestFunds
    , genRewardAccounts
    , hwWalletFunds
    , maryIntegrationTestAssets
    , mirMnemonics
    , seaHorseTestAssets
    , shelleyIntegrationTestFunds
    )
import Test.Integration.Framework.Context
    ( Context (..), PoolGarbageCollectionEvent (..) )
import Test.Utils.Paths
    ( getTestData, inNixBuild )
import UnliftIO.Async
    ( race )
import UnliftIO.Exception
    ( SomeException, isAsyncException, throwIO, withException )
import UnliftIO.MVar
    ( newEmptyMVar, newMVar, putMVar, takeMVar, withMVar )

import qualified Cardano.BM.Backend.EKGView as EKG
import qualified Cardano.Pool.DB as Pool
import qualified Cardano.Pool.DB.Sqlite as Pool
import qualified Data.Text as T
import qualified Test.Integration.Scenario.API.Byron.Addresses as ByronAddresses
import qualified Test.Integration.Scenario.API.Byron.CoinSelections as ByronCoinSelections
import qualified Test.Integration.Scenario.API.Byron.HWWallets as ByronHWWallets
import qualified Test.Integration.Scenario.API.Byron.Migrations as ByronMigrations
import qualified Test.Integration.Scenario.API.Byron.Transactions as ByronTransactions
import qualified Test.Integration.Scenario.API.Byron.TransactionsNew as ByronTransactionsNew
import qualified Test.Integration.Scenario.API.Byron.Wallets as ByronWallets
import qualified Test.Integration.Scenario.API.Network as Network
import qualified Test.Integration.Scenario.API.Shared.Addresses as SharedAddresses
import qualified Test.Integration.Scenario.API.Shared.Wallets as SharedWallets
import qualified Test.Integration.Scenario.API.Shelley.Addresses as Addresses
import qualified Test.Integration.Scenario.API.Shelley.CoinSelections as CoinSelections
import qualified Test.Integration.Scenario.API.Shelley.HWWallets as HWWallets
import qualified Test.Integration.Scenario.API.Shelley.Migrations as Migrations
import qualified Test.Integration.Scenario.API.Shelley.Network as Network_
import qualified Test.Integration.Scenario.API.Shelley.Settings as Settings
import qualified Test.Integration.Scenario.API.Shelley.StakePools as StakePools
import qualified Test.Integration.Scenario.API.Shelley.Transactions as Transactions
import qualified Test.Integration.Scenario.API.Shelley.TransactionsNew as TransactionsNew
import qualified Test.Integration.Scenario.API.Shelley.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Network as NetworkCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Addresses as AddressesCLI
import qualified Test.Integration.Scenario.CLI.Shelley.HWWallets as HWWalletsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Transactions as TransactionsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Wallets as WalletsCLI

main :: forall n. (n ~ 'Mainnet) => IO ()
main = withTestsSetup $ \testDir tracers -> do
    nix <- inNixBuild
    hspecMain $ do
        describe "No backend required" $
            parallelIf (not nix) $ describe "Miscellaneous CLI tests"
                MiscellaneousCLI.spec
        specWithServer testDir tracers $ do
            describe "API Specifications" $ do
                parallel $ do
                    Addresses.spec @n
                    CoinSelections.spec @n
                    ByronAddresses.spec @n
                    ByronCoinSelections.spec @n
                    Wallets.spec @n
                    SharedWallets.spec @n
                    SharedAddresses.spec @n
                    ByronWallets.spec @n
                    HWWallets.spec @n
                    Migrations.spec @n
                    ByronMigrations.spec @n
                    Transactions.spec @n
                    TransactionsNew.spec @n
                    Network.spec
                    Network_.spec
                    StakePools.spec @n
                    ByronTransactions.spec @n
                    ByronTransactionsNew.spec @n
                    ByronHWWallets.spec @n

            -- Possible conflict with StakePools - mark as not parallizable
            sequential $ Settings.spec @n

            -- Hydra runs tests with code coverage enabled. CLI tests run
            -- multiple processes. These processes can try to write to the
            -- same .tix file simultaneously, causing errors.
            --
            -- Because of this, don't run the CLI tests in parallel in hydra.
            parallelIf (not nix) $ describe "CLI Specifications" $ do
                AddressesCLI.spec @n
                TransactionsCLI.spec @n
                WalletsCLI.spec @n
                HWWalletsCLI.spec @n
                PortCLI.spec
                NetworkCLI.spec
  where
    parallelIf flag = if flag then parallel else sequential

-- | Do all the program setup required for integration tests, create a temporary
-- directory, and pass this info to the main hspec action.
withTestsSetup :: (FilePath -> (Tracer IO TestsLog, Tracers IO) -> IO a) -> IO a
withTestsSetup action = do
    -- Handle SIGTERM properly
    installSignalHandlersNoLogging
    -- Stop cardano-cli complaining about file permissions
    setDefaultFilePermissions
    -- Enables small test-specific workarounds, like timing out faster if wallet
    -- deletion fails.
    setEnv "CARDANO_WALLET_TEST_INTEGRATION" "1"

    -- Flush test output as soon as a line is printed.
    -- Set UTF-8, regardless of user locale.
    withUtf8Encoding $
        -- This temporary directory will contain logs, and all other data
        -- produced by the integration tests.
        withSystemTempDir stdoutTextTracer "test" $ \testDir ->
            withTracers testDir $ action testDir

specWithServer
    :: FilePath
    -> (Tracer IO TestsLog, Tracers IO)
    -> SpecWith Context
    -> Spec
specWithServer testDir (tr, tracers) = aroundAll withContext
  where
    withContext :: (Context -> IO ()) -> IO ()
    withContext action = bracketTracer' tr "withContext" $ do
        ctx <- newEmptyMVar
        poolGarbageCollectionEvents <- newIORef []
        let dbEventRecorder =
                recordPoolGarbageCollectionEvents poolGarbageCollectionEvents
        let setupContext smashUrl conn np baseUrl = bracketTracer' tr "setupContext" $ do
                prometheusUrl <- (maybe "none" (\(h, p) -> T.pack h <> ":" <> toText @(Port "Prometheus") p)) <$> getPrometheusURL
                ekgUrl <- (maybe "none" (\(h, p) -> T.pack h <> ":" <> toText @(Port "EKG") p)) <$> getEKGURL
                traceWith tr $ MsgBaseUrl baseUrl ekgUrl prometheusUrl smashUrl
                let fiveMinutes = 300 * 1000 * 1000 -- 5 minutes in microseconds
                manager <- newManager $ defaultManagerSettings
                    { managerResponseTimeout = responseTimeoutMicro fiveMinutes
                    }
                faucet <- initFaucet

                era <- clusterToApiEra <$> clusterEraFromEnv

                mintSeaHorseAssetsLock <- newMVar ()

                putMVar ctx $ Context
                    { _cleanup = pure ()
                    , _manager = (baseUrl, manager)
                    , _walletPort = Port . fromIntegral $ portFromURL baseUrl
                    , _faucet = faucet
                    , _feeEstimator = error "feeEstimator: unused in shelley specs"
                    , _networkParameters = np
                    , _poolGarbageCollectionEvents = poolGarbageCollectionEvents
                    , _mainEra = era
                    , _smashUrl = smashUrl
                    , _mintSeaHorseAssets = \nPerAddr batchSize c addrs ->
                        withMVar mintSeaHorseAssetsLock $ \() ->
                            sendFaucetAssetsTo tr' conn testDir batchSize
                                $ encodeAddresses
                                $ seaHorseTestAssets nPerAddr c addrs
                    , _moveRewardsToScript = \(script, coin) ->
                            moveInstantaneousRewardsTo tr' conn testDir
                            [(ScriptCredential script, coin)]
                    }
        let action' = bracketTracer' tr "spec" . action
        res <- race
            (withServer dbEventRecorder setupContext)
            (takeMVar ctx >>= action')
        whenLeft res (throwIO . ProcessHasExited "integration")

    -- A decorator for the pool database that records all calls to the
    -- 'removeRetiredPools' operation.
    --
    -- The parameters and return value of each call are recorded by appending
    -- a 'PoolGarbageCollectionEvent' value to the start of the given log.
    --
    recordPoolGarbageCollectionEvents
        :: IORef [PoolGarbageCollectionEvent]
        -> Pool.DBDecorator IO
    recordPoolGarbageCollectionEvents eventsRef = Pool.DBDecorator decorate
      where
        decorate Pool.DBLayer {..} =
            Pool.DBLayer {removeRetiredPools = removeRetiredPoolsDecorated, ..}
          where
            removeRetiredPoolsDecorated epochNo = do
                certificates <- removeRetiredPools epochNo
                let event = PoolGarbageCollectionEvent epochNo certificates
                liftIO $ do
                    traceWith tr $ MsgPoolGarbageCollectionEvent event
                    atomicModifyIORef' eventsRef ((,()) . (event :))
                pure certificates

    withServer dbDecorator onReady = bracketTracer' tr "withServer" $
        withSMASH tr' testDir $ \smashUrl -> do
            clusterCfg <- localClusterConfigFromEnv
            withCluster tr' testDir clusterCfg faucetFunds $
                onClusterStart (onReady $ T.pack smashUrl) dbDecorator

    tr' = contramap MsgCluster tr
    encodeAddresses = map (first (T.unpack . encodeAddress @'Mainnet))

    faucetFunds =
--        (maryIntegrationTestAssets (Coin 10_000_000))
         map (\(a, b) -> (b, a)) $
            shelleyIntegrationTestFunds
             <> byronIntegrationTestFunds
             <> setupFaucetFunds
             <> map (first unsafeDecodeAddr) hwWalletFunds

    unsafeDecodeAddr = either (error . show) id . decodeAddress @'Mainnet

    setupFaucetFunds = map
        ((,Coin 1000000000000000) . unsafeDecodeAddr . T.pack)
      [ "Ae2tdPwUPEZGc7WAmkmXxP3QJ8aiKSMGgfWV6w4A58ebjpr5ah147VvJfDH"
      , "Ae2tdPwUPEZCREUZxa3F1fTyVPMU2MLMYAkRe7DEVoyZsWKahphgdifWuc3"
      , "Ae2tdPwUPEYxL4wYjNxK8z5mCgMmnG1WkMFZaeZ6EGdV2LDZ5pgQzvzVpuo"
      , "Ae2tdPwUPEZMcoAHgC7RvCL9ewjZdj9Yrej2bHJJpvubhkSaRn5Y7dPGKRy"
      , "Ae2tdPwUPEZ7geEbqcaNfMFL8EMpeRYAQrHABau6xUmek87xeyyrmPm4ETc"
      , "Ae2tdPwUPEZNHxjww4RhosX3LMVAzbJtCj3vzoQM3wgLwhEHUp13jX8Xte8"
      , "Ae2tdPwUPEZ8cgFfwvjp9t42v3zQE8nCsjxMpDcdcJZzBocsUK2btirTHDN"
      , "Ae2tdPwUPEZK4VrjHdDpeTfSvWMzNa6qZ5erD2aVmU5S3mCeCZsoT6SJ6NW"
      , "Ae2tdPwUPEZ2pEgBhSNKiUXRfhb5p8jByYiJXAsokHdLGMVeqLjHFNaEr7b"
      , "VhLXUZmS1gXFnDcCzVHi2BqhkA1cvDUZrMvGfYotD4eEjKnkdfid7YsY"
      , "Ae2tdPwUPEYxYSimKRCvz9iqtsCEAeN6KR7SC1dWFYgCVb18ttTrJaht4qz"
      , "Ae2tdPwUPEZ16WMj3KGxQxTtm7cgY2oygWF8Pk1gWRCL9phsawFoJUQo8V4"
      , "Ae2tdPwUPEZ3S2LzBCw3v9qm7ZfADBeHa8GjC4g71bKLeS1HJiNPz58efsG"
      , "Ae2tdPwUPEZ5MEg5J9CJBuanYyoAeq8Usyeh3mTpAjFAfaMUHErZCC6VESB"
      , "Ae2tdPwUPEZKTEGqULNJggS2feij8B5DEkTgvj4pf6BX9xaNWsrk83a94op"
      , "Ae2tdPwUPEZ1x5d9EZgDis5f33LKFR4ZrGwh3uhYVYThiubgFSzSa5ZWWjn"
      , "Ae2tdPwUPEZLEiDLGWsbGYvnKQbDxJaUJ6PPx7ynjAjnLsNjsBB9qfwD8FL"
      , "Ae2tdPwUPEZEMR4QcU9rFCeTK8G6E5ABNAhiuEDzritQarbJ56GBMbPem8v"
      , "Ae2tdPwUPEZMgjLUEpnfpbaGrrBc3mcfLMgzT8JL2rsWcE8YGuwerng4JTx"
      , "Ae2tdPwUPEZCdpgB296udjjMqK4crPXjpMz9zzzk1QARbC844JqYGygKZck"
      , "Ae2tdPwUPEZC7DMJnx7xpRjG9wQXsNtCKvkB5RhDqK9zzra96ugUfMgkw6F"
      , "Ae2tdPwUPEZA2Hxg2X94qnx42UwLdnC2vfjSw1na2jcWnS2LjeoazWgcGqz"
      , "Ae2tdPwUPEYzwDXTM8VDDNG48ZVJPZT5ev3BGpLsBZqkYeP9Ay6keHQiUHN"
      , "Ae2tdPwUPEZK5jjAU6gc8o1Hxk9FGC2JXYR29eRj2zvYDVRy3oJKmzkkWXr"
      , "Ae2tdPwUPEZHRYGpLbcxzKSBFmVghBdUbMLD7Z1RP3CaWmE2MfudSCdLERE"
      , "Ae2tdPwUPEZ3YosvMkMYRuHAzGXmj9FDZiSWxZJxY2bfjtXQupV6cFufGxj"
      , "Ae2tdPwUPEZAUVNwHSzyz3RRhe9hgFNvw6ZBWgusousZEu71AUxwkjTJQXd"
      , "Ae2tdPwUPEZBWbsXKZ6Xj1hVqNrJevo1MguQErP7Ekws9Mwe3QyApRbfzuj"
      , "Ae2tdPwUPEZBwEwpyZ86qJJ5UcBs7zENaB9JmB1ccKKrjF2m8WqYvRLQTUQ"
      , "Ae2tdPwUPEZLVrvsAkoKffT5T2Ny9peTcw1pgDQZGUNuyhsShZYRGdJdg3P"
      , "Ae2tdPwUPEZMMcjnYLD8hNzD8rBuQX4Rbwh4Hrri9wo9Vd3QhWgJp82Q3Zb"
      , "Ae2tdPwUPEZNCXJnNKSoVwATYNRoehHnwhQLeg7Voeun7aKgw7pBELp9Xyx"
      , "Ae2tdPwUPEZMZgPQpYm9VNwW6o1y9gtgmmuto8XxnVzJQnQWNyfbK1ehxhG"
      , "Ae2tdPwUPEYx5Boej5GuTgWrL6yhioVeAN9KybWPCZgfbzTNfE4p134zvFr"
      , "Ae2tdPwUPEZAGMrgFKgSjDymZ6bRhcuCgK53xX5n7xcDUHC8MnijrSVU69g"
      , "Ae2tdPwUPEZL7g7DTRjBp63JMbSouTPJcjjZD6GQCiK3HseKbs2AYHLwcUk"
      , "Ae2tdPwUPEYw3nfF8ceQBJZ3zFL4jP9SFoyJ6N1qYTj6fk1SLaxUhrYFqAp"
      , "Ae2tdPwUPEZBWq2xEQD7NacM1cmTAvnRdwnLX5jGkBvvZpjBCCaTyVbQyCg"
      , "Ae2tdPwUPEZ2BJqnSoUrhVQ4Nf5XmHP6beK1LvYrZFaJqG6PLbHtEKzQCFV"
      , "Ae2tdPwUPEZLGkJsDc5t8WUgPafrvpQkTjXhc3zwZfT2RRSD2SCDwGJ2gko"
      , "Ae2tdPwUPEZG48xoQbHyjEw4sAz4KFFPC6H3RjvZoqDd7ui1hnBoCZ7hjZK"
      , "Ae2tdPwUPEZGjAkaWbCogSWVBjhUxnF2sMRq2QUu82itFU4PAcdo8NkLBGx"
      , "Ae2tdPwUPEZGUUmRGEwhKYoGtuqjubky2tQDB4b59RVsEaMedoNjkgBhz3z"
      , "Ae2tdPwUPEZD4CQHEa9YBp3FgK15dbM8wE4i6VcZczaUNix8U1rnrxrTBqe"
      , "Ae2tdPwUPEZ8uESNVsKkobHzoEZeRpmim475QdWF6CmBdJHWFSJjo9BT5s2"
      , "Ae2tdPwUPEZBhxiuQ3tnhdh5mW8PS5yAJ8jsxYbhs6PvYPx11o7eBs2Nja1"
      , "Ae2tdPwUPEZGXi9taRWo4pYMMZ9WtvvJme3yhmi61PkZEPUaE5c4GhwPVim"
      , "Ae2tdPwUPEZMCPdErTxmgUT4FbQty7tcCmHidJkTAxMpYGF6RYVNkrK1JAR"
      , "Ae2tdPwUPEZ92FRSRqV4dz49btBPRJUEhzyCN4Yh3QZmxGjkD18VxtAvjrJ"
      , "Ae2tdPwUPEZHto9s5ouv4SQha5WpwNrEERfWQDerXgxygM2exm9MSH972o2"
      , "Ae2tdPwUPEYyg77BWtM7HDR9DgtntvnjD5sANzHsXhLSrfHw2QoYnhzVkBV"
      , "Ae2tdPwUPEZ1SBb6wXc9WP5DY3PGRyh6puiaFCUG8mvwPsfijvDvE3FtYV3"
      , "Ae2tdPwUPEYw7n23qBj9dxeTk6vNjGwzHfSXx1zzG1k98smReGMGZmCdwvD"
      , "Ae2tdPwUPEZMsinkhpKJy3yYQ2f486UC1f3iLfeCntEe2AgyWkp3sMxXUZB"
      , "Ae2tdPwUPEZ8V56xa8NY8yAz6pbpyzmbnwneqmHJxoHisXyiiDSubsSDqTY"
      , "Ae2tdPwUPEZNCgK9K9CD9B6c1BcVMcJbSLhTBwNDWzhQ265zrYEjrV47eeW"
      , "Ae2tdPwUPEZ5PXtvRfwrrGa9ZGcmApTwTqvh58QTQANDX2ddLUcpTZnaHLo"
      , "Ae2tdPwUPEYzVh39uUKFBSubv4FGenCAEyV2BdKSwCADzVJYKEJVwPAUicj"
      , "Ae2tdPwUPEZCT2LnNBam5QjU6LE5VQRS7Z2JW1md69zMvu9y9WMnLwN3bX6"
      , "Ae2tdPwUPEZ8AFCshDagF6igZf2bHXixA1g5PdpRvn4KyTpG6zyMzky4ehh"
      , "Ae2tdPwUPEZ6nWqtXbKtchU3mpyRtrRZDt4obySFrrR85M4XcN74KTktXKv"
      , "Ae2tdPwUPEZMigfySnz9UFSmmMYvRUd2kPadT272pbbHotNVRp2scDyG2AK"
      , "Ae2tdPwUPEYxiwE99mBo8SkNPkzPEgrJmZpyXd9RuHWhpGKrSYaxUcKAbYQ"
      , "Ae2tdPwUPEZ9jpF2FAh8dxQ3BCWgG19ThVYPkEyMjhThvrhXx8ngBQeHhCQ"
      , "Ae2tdPwUPEZ82cmCBfjYq8iRzRWGgjMs7UkPypwp8LiSUJyMFEJGxBr2YKq"
      , "Ae2tdPwUPEZ1eMNrx76WA5JBwvxiHQWxM3tNYjpFDnJp9fgq86BHcxqSfN4"
      , "Ae2tdPwUPEZKJUFkpxqYrE32biZKQuqgWUdNKhFWbrGxJCnUNXVaxtQkErR"
      , "Ae2tdPwUPEYwAGnLtgusi3JKq4mvNqWvY9aztGtLwa22ko3HzUra3hjGXGx"
      , "Ae2tdPwUPEZ81XjXQAzpCj6QkV99kgkK46aS4J8xfppMi3R2Dpq4hhk7VNE"
      , "Ae2tdPwUPEZ7nPhRYqbcNaaif222Dp9rx998Q2YGYR2UNxw8qmNWwJ6daxo"
      , "Ae2tdPwUPEZ43xHeJbzVkx15t8qAhham5nt72JeK6XpXYvm68bfUHk6uVju"
      , "Ae2tdPwUPEZD45f87j3XvfwTWfTNgnz8QpnksffePU32ivaifqxcENuG6KK"
      , "Ae2tdPwUPEZF42GYPd3j7iw2cCUEMvirSk4vLPkTRdqqJtr4R4PsHSj4w2d"
      , "Ae2tdPwUPEYzyxBezBeDqDzfNQ3gzF27LVvAqETTsaw6kdJpTWHCgmPVEo2"
      , "Ae2tdPwUPEZGXRwDFR5VCmKCesFgBqgtrADgFo9FfjwSPEAyJvtVfh1JSmX"
      , "Ae2tdPwUPEZMYDvawa3S1DCA7eZdhrDFJMXHyh5hpxZJCQJD8c6ruBRanDJ"
      , "Ae2tdPwUPEZ8ffskBQYLzjPyqyxKsiNzYbvcJSN9JintHx6V6K1K8aEtho5"
      , "Ae2tdPwUPEZ8cmT88Unk2WD5YzUCcc8ifb3SzMQMpj5LS1QgRa7g6kez46h"
      , "Ae2tdPwUPEZGqtA4AbujDXkMH6zFZvTjUnRajLtwTCRV39EVdYtQJKrsc8u"
      , "Ae2tdPwUPEZ5oH337RvQhYkjaDjvZnK1PKD4tVsJsNKcBcGUWihgTsiVtde"
      , "Ae2tdPwUPEZAKA1vGHeZVpa3zhakExJ5utM9vwJ6auahoiCNFf6SufibHpC"
      , "Ae2tdPwUPEYxkHxX8KdWAPkfkTxa8kdNaZEo69baccQ7HpRfUUsELigZJf4"
      , "Ae2tdPwUPEZHajXavDF4CN4ExxHJUof8A2N2ugdEhv3LuPb76YmgUhxPu8R"
      , "Ae2tdPwUPEZGpXcqTCfq9KocPWYgVB234GRUdFVDhnxJ2H9stGrszkZJKTc"
      , "Ae2tdPwUPEZDVJUU3NfXH8di6D5E16djtgaFjWm8f81CEmoHUnMwMGGqbVj"
      , "Ae2tdPwUPEZAS8cHTvHVwgPoAC1dg9RdTx3nQVam8gNebLYwiy9YccQQuB1"
      , "Ae2tdPwUPEZ5hLgiaE7dzZuhqo68xZ7sMiqMGp39auHPcsE1VNNRvq7PnYN"
      , "Ae2tdPwUPEZAdY5hGCpQpxT2ReHdW8gd3A4h5CJsedt9SyQeUpHBzzcwjAt"
      , "Ae2tdPwUPEZ4afabfMLDJbX7Gaazj71zPpPrLeNywrv8uusU95bm21CBnwE"
      , "Ae2tdPwUPEZ7wwdAXP8z1hhMMWNrP9cc34eCFPbvEi5zFm6jDunvFq74WZe"
      , "Ae2tdPwUPEZMNyJAuNPb76ejraE3j3vQTup1xRxBHa5fKgzfznWbJijt5q2"
      , "Ae2tdPwUPEZHSzjcTUtJGNw5EcMtoYcEMpmdiPAMn1HVzy52WoTtRFpukws"
      , "Ae2tdPwUPEZMZLrkwBYumeF8P8eDPzRUWmW2epZRGRiGcvkhQptDFbujuQq"
      , "Ae2tdPwUPEZ56rfrz5TdFY1JHnCkTGMWRX4orh6Q1BMmTV5ATx7z4xbFfG7"
      , "Ae2tdPwUPEYyV78NYSddi6atWJgjWTpBHC3J1H2ceXzbDd5znBchmyp7sV3"
      , "Ae2tdPwUPEZ9jb4o5V26jQKbeDkppnJkgebXbWaabndYsRnXXYVb6weu2BP"
      , "Ae2tdPwUPEZHVs5JvSXmYxYvZGHZ8DHoM2zfJaiL99LkRbnvpH3oAVKuoS5"
      , "Ae2tdPwUPEZ967PQDmUALkQ7cEuuQVdCQp1iuUXnpbgE1kzamaBJ7qpqkwj"
      , "Ae2tdPwUPEZA8i4pSXDVJHTufffv59optZ9CFbfdUgJbHqUYbdx93N7ppV9"
      , "Ae2tdPwUPEYyDqAPnJ18XPaTE77vDAeuVa4Ytp7GBNe9PNvNLeLVBiM4jVL"
      , "Ae2tdPwUPEYw1wgtGgnoe2NbgfoFyxERny8qJM1vkqCXzkiXipJkJ7qvoR9"
      , "Ae2tdPwUPEZHKcKbatmsP23ACD6VVXiNa9czTngsBnHGT5dqqi233xVLcGs"
      , "Ae2tdPwUPEZEapggvTWfEx5jK1kkGVYMKeex7DcJVcTgmKxdcUnQXrDho2b"
      , "Ae2tdPwUPEZ1NPbZE91PQidZVBafLLco2YnpHdgwTxNPKgygXSwZVq4dgKB"
      , "Ae2tdPwUPEZLVnbtDRzNT1WmVfHTrkPs4JG38xNfmGkNWV9WgxYriy1qd6o"
      , "Ae2tdPwUPEZHUxRcryapNJoL8Fo6kMGFXsLQSLC3nmhbpz3M6RaT3CcfKrZ"
      , "Ae2tdPwUPEZ19YqjHnDr1yckaWEjwtZoaC3HZpVHepyzvcrVFtFoBUx4y1P"
      , "Ae2tdPwUPEYxdvmBHt6hD1ra9DwYMUed6VT3aB16DA8VZWGQvJyhd1MJSkE"
      , "Ae2tdPwUPEZ5grUgBooGGbBK9yHqdgVTdECqwS2XaeqG8boGBGqCA3nSBDi"
      , "Ae2tdPwUPEZLSj5xiNKzbZXQ2ZjKU4JLyfvf5E7dQLahcGZZg4QA7pNVZg2"
      , "Ae2tdPwUPEZHAvgfBNo8va259BSfq8nZpC7Lwp8jMJHkkUppMQnpRgPARaL"
      , "Ae2tdPwUPEZGNCsJF8xVNjHYAKDkyerXt2wCRexy7BFXcWvyiHFKSHTPJdF"
      , "Ae2tdPwUPEYzo3JzNowvs4gS69rZ3R5nT2KKZKWWxaymCufUsatVpu2kqii"
      , "Ae2tdPwUPEZFu8H46FK5q7g6ApMFAqpoYJJjmLyh8DheUL51i5dhbLcmSXG"
      , "Ae2tdPwUPEZ5fTgRDV736NaHHUAKaxj4ytyX1j7NLAtAF3x7gtUFGc2L8U3"
      , "Ae2tdPwUPEZCwt8ZP7R3wHB2Doed6neUHmhZYERTh3bsTQm6EfjFcfWmnTc"
      , "Ae2tdPwUPEZFQYXdB6V3wPfh99fDb8F3fXSvjVu7qBSjP8kVf81H2ApkaQu"
      , "Ae2tdPwUPEZEyVBVWrGSbQqrzQgNEdLexbUZJzqkF95Co3eESSVxerDdUfS"
      , "Ae2tdPwUPEYy6cvJ1mo5fBhYvP7r6RTpmxNGBgX8Cs4FC39eJr8DWYMd9vv"
      , "Ae2tdPwUPEZMQjnsmRoq1Vxb31PfLhxaBLsorC38QYj8Qbx9Afqg9DNeJhc"
      , "Ae2tdPwUPEZEpQ5obkgfFrjXk1GKnNBg7fkyjmNUhkH3vBxmZw7menySh28"
      , "Ae2tdPwUPEZ4hwGffsjLTTApiZEK1HgaVnndfJA1az5ToZNhiieXoskiixx"
      , "Ae2tdPwUPEZKzTzbEfDkNLvM3AfzMASBWmcSM9EU5aZ2iAAyuoyQd2gyNNN"
      , "Ae2tdPwUPEYyK9ph2bLu4GwopB38aUoHBDG2zDYGfdbZCEfYFXv6NDix979"
      , "Ae2tdPwUPEYy9WUnYWknL4SWq2nF8y2L7FngyhV6ftMEQYaTAtCxVjWHMjo"
      , "Ae2tdPwUPEZKgCUPxD5tSUDtgn3PiTfenMAFcTEBXsJqiESDmQnzxCVJj7B"
      , "Ae2tdPwUPEZ8uuaUYL4GD5uS5yiUTW6JYW54K258EGFyDeFK465fPXb2dsB"
      , "Ae2tdPwUPEZBhevhLwkd7maXseXHSfJMwgkNNraPnBXh1w86dChTRbDgrEr"
      , "Ae2tdPwUPEZLEdZb2Un8b2JLfRXzQi3cYbAtn4NG6SmLYiv1vxueuESNFVr"
      , "Ae2tdPwUPEYwpmuPpqUeqn2qTc3xEY6siqmTTaC6tn5S6fb45d8gz7Pdje3"
      , "Ae2tdPwUPEZCTzw5sgjL8X51m7Dg4xccizqJFRnrwyEWByTE4WTt1BnqtbA"
      , "Ae2tdPwUPEZ7tTXxGa4WfnGbN7qJu8gSRMmsjTDgNhz3qdCiuYC5N3ZMR12"
      , "Ae2tdPwUPEZ1UZJcQUs61oXayVvQVKAsry9oMMgDwSK9z2eMw8DibHsap1f"
      , "Ae2tdPwUPEYwJDXVgaPdZoFmDm2PcwqY67xBDpnj4z3UJmfR9dMD2XAfCjw"
      , "Ae2tdPwUPEZKr5rmjQY7aFHgEMAbMqtV38XtJCZtdNFKoiPVnWLnNDf4BGp"
      , "Ae2tdPwUPEYzSnRmYNX9GjEkhc1gXewiS2b3XQyMjztyiWrZiA6AdtWzpQ4"
      , "Ae2tdPwUPEZ4tThjhRaZZxAT1SNfRfB7yt9gYCysSamKkB7HUVH7NjkWxaA"
      , "Ae2tdPwUPEZ4msp1fbqK25ShSJ4BGYq6QbhBf4ALi3i17JS7KCx7gA8ksG8"
      , "Ae2tdPwUPEZGrBvM4Qr6wiWTMbJ7W46cMLWsenw3JQ9WvH7xwVnJTkL6n2Z"
      , "Ae2tdPwUPEZ9fUaqXRMUXhpwAqoGSaSXcrUGByyGyUnHokYH3dt2FBD8BLS"
      , "Ae2tdPwUPEZFbSUYiJG9oxa1U97ypoRHr7xg2PBhbXWShLRRU1Mav1tyYSw"
      , "Ae2tdPwUPEZJ6JcaDPLRZBNLyyB7QfN5sm1TGPpC8BCVF9eezeyRiPRXYHH"
      , "Ae2tdPwUPEZE5ZueRGyhkaW9qwWMiHYVM9uN8iTKYtTLoYoaEEU4djnKShk"
      , "Ae2tdPwUPEZJkqt5PS6o5myu5H15Gje6cPwJYXHN1ji4BzPiTKXzBvXjhWy"
      , "Ae2tdPwUPEZ1v2xoxVpm3pxFw5U6WuRV4Q3kdivrWF5cUhTVPgkBm8kMRvu"
      , "Ae2tdPwUPEZK1afLbsLTMb56F3MPCqqTq78ygzbZAamrExQMvSgyUT6jHPF"
      , "Ae2tdPwUPEZF2oYZxKaMntEh48gFqPKoGhjAaQwVNQMmUa695mhjQmebnkq"
      , "Ae2tdPwUPEZCsnxYXZfzXmbfuiBse9tTTimUuqEv4BRHjThCA4igaAfBmaN"
      , "Ae2tdPwUPEYw34SJK5vkreGkV9AUmMUB1pN9bcCjk8H3EVMbbw2PcjubFCq"
      , "Ae2tdPwUPEZLTWD9YuWFQTzLCZAbqnHwui8QSPPYAeNC7BobRVVajMsBgM1"
      , "Ae2tdPwUPEZ8UWnc14XpyhupmGrNk9QeguBfW8gzQ8WZ6PcUAtCgBdyCxsW"
      , "Ae2tdPwUPEYxzJRUWjG2e8FytD24VNa7FVYr4cdMmPBjoe3MCVVsvpHyh55"
      , "Ae2tdPwUPEZL14t4gybitgy6eHHogQUJS5pRH6P74fDeWuA8p76pMGnNBCR"
      , "Ae2tdPwUPEZM7EpvTXRV9ynN4mzoYFgG9xATWqEofbw2ZVK4AjALqaZxU3H"
      , "Ae2tdPwUPEZAXXviL2b9KNt6a5uHH5x6d3pzdPVCheXBRT81XrAKK2qMqtg"
      , "Ae2tdPwUPEZ3VrxgvtfBz2JXuszTPAKCLfapzcusf9zmxqWKxorW95QxEcR"
      , "Ae2tdPwUPEZ2t7h2auTtCbyoBk7uvroZQQ4ns5D6xoUAX83b72qqYJZDqgs"
      , "Ae2tdPwUPEZDpPM7EhAw1XVzRS52KHxASnkDceu6XTHuCJ3sPHFeCd6NDyZ"
      , "Ae2tdPwUPEZ73MuSt6NBpTSU4dzMpU2Lcd7jaKYnhfT4wS7udiB2ygy7znp"
      , "Ae2tdPwUPEZ3b8rdA63Qnvs6TGtmBaoNUXtf7vkYfUSf4iABUsWyFewiNav"
      , "Ae2tdPwUPEZHj8Kjyc4mbww3CRXBqjYhmKiXXyesGuCJZbffBFTyYWg54LE"
      , "Ae2tdPwUPEZMYomeS16gfhsV5UPuygbfPPRpMZiUwUmSxeHquue5VBiiXUs"
      , "Ae2tdPwUPEZ9TrvR9uzKnJZkxvPeTPMXB5EHkBhSb9odZa6z6RKKj3pSrrw"
      , "Ae2tdPwUPEZGAkywA1EDCnE5dTqKfx5Ngf6nbMbCmUWpRirKLv1Rp68eFwP"
      , "Ae2tdPwUPEZFjizwxcB6U2g5nwpkquqFQL78E7wq4mRp8JbQd3etaDyn1R3"
      , "Ae2tdPwUPEZ5Zznsim2RjRnDwo2CNQdTiQgKUWwED3v97qksmDnefKcGjwB"
      , "Ae2tdPwUPEZFAkbyARmyeFMR4c5yikc4AySUosnJWdw65FxJ6AsL7wh6XnJ"
      , "Ae2tdPwUPEYw7i4tXgdRBNAMVqTfskTUFTRYaVQoGyLnM87tXKuVodcUTmo"
      , "Ae2tdPwUPEZ7YLaEDbGKpWn6Ds5dRomUJ93aEF3Ptc6kkEq8Nxes118czAJ"
      , "Ae2tdPwUPEZ3pbYRkq3M3BDuLp5JLA5pBiT8diXZy8tec8FKtgdiQpS7eM2"
      , "Ae2tdPwUPEZ5kjhAsNtPK9sA4Kj8cLnmZV63RNGPXimMAPib3vPScuSRfFQ"
      , "Ae2tdPwUPEZAgEaoWowXz8w3K5agdtukBAYCpeR9o37e8rogzrhn8t8SDdi"
      , "Ae2tdPwUPEZMYomeS16gfhsV5UPuygbfPPRpMZiUwUmSxeHquue5VBiiXUs"
      , "Ae2tdPwUPEZ9TrvR9uzKnJZkxvPeTPMXB5EHkBhSb9odZa6z6RKKj3pSrrw"
      , "Ae2tdPwUPEZGAkywA1EDCnE5dTqKfx5Ngf6nbMbCmUWpRirKLv1Rp68eFwP"
      , "Ae2tdPwUPEZFjizwxcB6U2g5nwpkquqFQL78E7wq4mRp8JbQd3etaDyn1R3"
      , "Ae2tdPwUPEZ5Zznsim2RjRnDwo2CNQdTiQgKUWwED3v97qksmDnefKcGjwB"
      , "Ae2tdPwUPEZFAkbyARmyeFMR4c5yikc4AySUosnJWdw65FxJ6AsL7wh6XnJ"
      , "Ae2tdPwUPEYw7i4tXgdRBNAMVqTfskTUFTRYaVQoGyLnM87tXKuVodcUTmo"
      , "Ae2tdPwUPEZ7YLaEDbGKpWn6Ds5dRomUJ93aEF3Ptc6kkEq8Nxes118czAJ"
      , "Ae2tdPwUPEZ3pbYRkq3M3BDuLp5JLA5pBiT8diXZy8tec8FKtgdiQpS7eM2"
      , "Ae2tdPwUPEZ5kjhAsNtPK9sA4Kj8cLnmZV63RNGPXimMAPib3vPScuSRfFQ"
      , "Ae2tdPwUPEZAgEaoWowXz8w3K5agdtukBAYCpeR9o37e8rogzrhn8t8SDdi"
      , "Ae2tdPwUPEZGBDWYqP7EFf5xABUf48zeupxgQ5wcwyE4hnLqrWxwv4FKZ4H"
      , "Ae2tdPwUPEZHkJRxkXZw7LiwD36VbQcz6ezrh8NxMjF5YZDpk8y5T7AqkbN"
      , "Ae2tdPwUPEZLXBf4ZiyWdBnjVdJj4mq36KzW8LczBzaWysiLXqv5iEvH8a5"
      , "Ae2tdPwUPEZGfG3euqbHvWDx1amXpngGgnXeD1Xehfi6SsRvijRwmUQbVzG"
      , "Ae2tdPwUPEZ2d3hdaPhgAn4M2qQ1YwkVW1JR5fXBmZqjF67n8AEyXy699FN"
      , "Ae2tdPwUPEZNEuvLyVeVnzGqz8RZRqszCrJtkDzyFNEWYWbK1sJrkg2noyR"
      , "Ae2tdPwUPEZ3huRFSrKKUj6cxmjPdxzrE4QgL3FjMNkUyqsCp6rqg35JiZJ"
      , "Ae2tdPwUPEZKYLBpCCsCnzRRiLcJ9W3zktENcBhCPg3GDqy5vvF77RE8EQW"
      , "Ae2tdPwUPEZ8BPPnf5dgoj9RAPBqZkKD2BtLPXQs1NcaKfPJ9xpRFukcx2v"
      , "Ae2tdPwUPEZKd8dcsyY5NeW7rAgMwA7sUTDwmqieYgeZoExZvxbMPnQfVFp"
      , "Ae2tdPwUPEZLMpPv3SoyV5SPqcvE9wAdk9H5iTmksEAn2p21eXGqCFTutxX"
      , "Ae2tdPwUPEYxbWadLJR8sd9WyJGYMvk5aZ5yAprWgwbfmXEZqJNguFwzpMN"
      , "Ae2tdPwUPEZ4xsrAWyHz4nHgC5RoffZZxHApRtx815m3en8M1n7JXynwhWd"
      , "Ae2tdPwUPEZ49twXRg8MMnYeqTYbcZekaRDLEYqqzZN9zTJtvNz8n7USJc9"
      , "Ae2tdPwUPEZ1qkgyJ3RqTmdnBGrVUEq5uHcSPvz7rHM8xKfGk9ZEydny8kH"
      , "Ae2tdPwUPEZ3H5CCbDTs9hby6fE474QpHjaPFtRHtxQ3maG7fmav1b7nNjg"
      , "Ae2tdPwUPEZJ9V14gEp6fEY94RsP6DMwQAxCK31h4nFHqpJfXZ9gzdZZRGz"
      , "Ae2tdPwUPEZKaVojFd7YhtbPcgMWtUzA2xXeyww9WyfhksVw1QUFyCpR5sd"
      , "Ae2tdPwUPEZHy5iKqn68XqGAx7wx5tdHchkCS3QY7zrYmZ3EBm5hUwJSkUb"
      , "Ae2tdPwUPEZ7Wo53F3GTJ93YzeLoJMJpvXirkCQcwGQafJrpTRZ1UmgL7LR"
      , "Ae2tdPwUPEZ9YgYPcYWGxm992Rsj3HSeGi7DiKLGxUfyRuNrMKb2k5fKR56"
      , "Ae2tdPwUPEZKR5s691Hpn5TAWVxRTnHae7U6wLD9giUutRaGiXp39PbHnSV"
      , "Ae2tdPwUPEZHywzbLni3qBUV3mCfAsfgnCdK1pBTRht1Q79AzfUS4mJ161E"
      , "Ae2tdPwUPEZEUS1HZBW2WLibjrCQvSx8smr1UuQT86Wc7osVrAdkmMZwEkH"
      , "Ae2tdPwUPEZ2vwANf3pV4YX2q3JpP1jGozyToLgRJWJY7EU735uoach8iPE"
      , "Ae2tdPwUPEZM2zssBS1PM34jrJEvms6badKtKzVzUzL3p5PavuXna5jUzeu"
      , "Ae2tdPwUPEZBAwPn77EhvqdABbAeBLuknY98CHX5GqRZDxbrrYjAURjh5iA"
      , "Ae2tdPwUPEZGKHFUV3QgGyx6quKEQhjk3YacFMgZ6k39Zf6R9scN239rD7q"
      , "Ae2tdPwUPEZ9GFCNDtgbKEnbC3qBoBCFYyFLbJHNscGY5LgJMm8UMYzGkTh"
      , "Ae2tdPwUPEZN7UdsESqCofiHSJCBGzbW8hrXGtPjAdVyzDxyBMxUwKqFoYU"
      , "Ae2tdPwUPEZ4WcYSHRLwM7zPdh5z1pWYBFJAPD7NsRSPEWN12gmysETSGmX"
      , "Ae2tdPwUPEZNLpZzpi6raWCGgqxf9E5tGoYSWEpuRm4RM6bXsV3G4rUPF3G"
      , "Ae2tdPwUPEZ1J7zvE2ZC8WqCsijgQdm1ZUwkdLnRTBfXASKFou5L29NpLKs"
      , "Ae2tdPwUPEZ5L17NbihRn95WXSo4YBN7vv4FGdNA5X84mmbviGpM9Ma67aa"
      , "Ae2tdPwUPEYxPxoQL8DrcchoY2gsxeK8JX3RSYGCUBY4xZH7yAaPjXrexDt"
      , "Ae2tdPwUPEZG4V4GdZBd93TaVpQEcGNBuQAJSK2yGVQg4x4EwXZ9gU3oYQr"
      , "Ae2tdPwUPEZKxg6sc6eEjLyau3wTYnZaAmKVn9a3apPtEcrg7ibYZzQhfdt"
      , "Ae2tdPwUPEZEAQJxUj5Xkcukd5mvCwrMuicspyAiDuPkxA598NJGrpRdnG2"
      ]

    onClusterStart action dbDecorator (RunningNode conn block0 (gp, vData) genesisPools) = do
        traceWith tr MsgSettingUpFaucet

        -- FIXME: Setup reward wallets
        --
        let rewards = (,Coin $ fromIntegral oneMillionAda) <$>
                concatMap genRewardAccounts mirMnemonics

        --threadDelay $ 4*1000*1000
        moveInstantaneousRewardsTo tr' conn testDir (first KeyCredential <$> rewards)


        sendFaucetAssetsTo tr' conn testDir 20 $
            encodeAddresses (maryIntegrationTestAssets (Coin 10_000_000))


        let db = testDir </> "wallets"
        createDirectory db
        listen <- walletListenFromEnv
        let testMetadata = $(getTestData) </> "token-metadata.json"
        withMetadataServer (queryServerStatic testMetadata) $ \tokenMetaUrl ->
            serveWallet
                (NodeSource conn vData)
                gp
                tunedForMainnetPipeliningStrategy
                (SomeNetworkDiscriminant $ Proxy @'Mainnet)
                genesisPools
                tracers
                (SyncTolerance 10)
                (Just db)
                (Just dbDecorator)
                "127.0.0.1"
                listen
                Nothing
                Nothing
                (Just tokenMetaUrl)
                block0
                (action conn gp)
                `withException` (traceWith tr . MsgServerError)

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data TestsLog
    = MsgBracket Text BracketLog
    | MsgBaseUrl URI Text Text Text
    | MsgSettingUpFaucet
    | MsgCluster ClusterLog
    | MsgPoolGarbageCollectionEvent PoolGarbageCollectionEvent
    | MsgServerError SomeException
    deriving (Show)

instance ToText TestsLog where
    toText = \case
        MsgBracket name b -> name <> ": " <> toText b
        MsgBaseUrl walletUrl ekgUrl prometheusUrl smashUrl -> T.unlines
            [ "Wallet url: " <> T.pack (show walletUrl)
            , "EKG url: " <> ekgUrl
            , "Prometheus url: " <> prometheusUrl
            , "SMASH url: " <> smashUrl
            ]
        MsgSettingUpFaucet -> "Setting up faucet..."
        MsgCluster msg -> toText msg
        MsgPoolGarbageCollectionEvent e -> mconcat
            [ "Intercepted pool garbage collection event for epoch "
            , toText (poolGarbageCollectionEpochNo e)
            , ". "
            , case poolGarbageCollectionCertificates e of
                [] -> "No pools were removed from the database."
                ps -> mconcat
                    [ "The following pools were removed from the database: "
                    , T.unwords (T.pack . show <$> ps)
                    ]
            ]
        MsgServerError e
            | isAsyncException e -> "Server thread cancelled"
            | otherwise -> T.pack (show e)

instance HasPrivacyAnnotation TestsLog
instance HasSeverityAnnotation TestsLog where
    getSeverityAnnotation = \case
        MsgBracket _ _ -> Debug
        MsgSettingUpFaucet -> Notice
        MsgBaseUrl {} -> Notice
        MsgCluster msg -> getSeverityAnnotation msg
        MsgPoolGarbageCollectionEvent _ -> Info
        MsgServerError e
            | isAsyncException e -> Info
            | otherwise -> Critical

withTracers
    :: FilePath
    -> ((Tracer IO TestsLog, Tracers IO) -> IO a)
    -> IO a
withTracers testDir action = do
    let getLogOutputs getMinSev name = do
            minSev <- getMinSev
            eraStr <- clusterEraToString <$> clusterEraFromEnv
            logDir <- fromMaybe testDir <$> testLogDirFromEnv (Just eraStr)
            pure
                [ LogToFile (logDir </> name) (min minSev Info)
                , LogToStdStreams minSev
                ]

    walletLogOutputs <- getLogOutputs walletMinSeverityFromEnv "wallet.log"
    testLogOutputs <- getLogOutputs testMinSeverityFromEnv "test.log"

    withLogging walletLogOutputs $ \(sb, (cfg, walTr)) -> do
        ekgEnabled >>= flip when (EKG.plugin cfg walTr sb >>= loadPlugin sb)
        withLogging testLogOutputs $ \(_, (_, testTr)) -> do
            let trTests = appendName "integration" testTr
            let tracers = setupTracers (tracerSeverities (Just Debug)) walTr
            action (trMessageText trTests, tracers)

bracketTracer' :: Tracer IO TestsLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)
