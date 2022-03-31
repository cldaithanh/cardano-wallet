module Light (
    -- * Overview
    -- $overview

    -- * NetworkLayer — for wallet
    NetworkLayer (..),
    mkNetworkLayerBlockfrost,

    -- ** Blocks and transaction data
    discoverTransactions,
    lightSync,
    BlockSummary (..),
    ChainFollower (..),

    -- ** Stake pools
    StakePoolsSummary (..),
    RewardParams (..),
    RewardInfoPool (..),

    -- ** Submitting transactions
    SealedTx,

    -- ** Reward accounts
    RewardAccount,

    -- ** Network information
    ProtocolParameters (..),

    -- * NetworkLayer — for stake pools

    -- | implementable
    getPoolBlocks,
    getPoolHistory,
    _blockBlockVrf,

    -- * Types exported for readability
    Coin,
    ChainPoint (..),
    SyncProgress,
    Pool,
    Light.Types.PoolId,
) where

import Control.Monad (
    void,
 )

import Light.ReadBlocks
import Light.StakePools
import Light.SubmitTx
import Light.Types

import Blockfrost.Client as BF

{- $overview

This modules presents various data types used in @cardano-wallet@
and checks whether they can implemented / filled with
data provied by http://blockfrost.io

Possible statuses of implementability are:

* done — this function is implemented here, in this prototype.
* yes — this function can be implemented,
  but was skipped as the implementation seemed straightforward.
* implementable — this function can be implemented,
 though may require additional thought or queries in cardano-graphql.
* __no__ — this function cannot be implemented as is.
* (internal) — this type is shown here for readability.

__Caveats__ are highlighted in bold.
-}

{-----------------------------------------------------------------------------
    NetworkLayer
------------------------------------------------------------------------------}

{- | Interface for network capabilities.

 Fields are listed in order of perceived difficulty.
 Some types have been changed or removed for clarity.
-}
data NetworkLayer m = NetworkLayer
    { -- | done, but only for __sequential address derivation__.
      -- Also, __payment address__ needs to be queriable directly.
      chainSync ::
        ChainFollower m ChainPoint ->
        m ()
    , -- | implementable.
      -- Pool performance estimation can be copied from ledger code.
      stakeDistribution ::
        m StakePoolsSummary
    , -- | implementable, but it is unclear whether we can be __notified__
      -- if the transaction is rejected by the node or times out.
      postTx ::
        SealedTx ->
        m ()
    , -- | implementable, redundant.
      getCachedRewardAccountBalance ::
        RewardAccount ->
        m Coin
    , -- | implementable.
      fetchRewardAccountBalances ::
        RewardAccount ->
        m Coin
    , -- | yes
      currentNetworkTip ::
        m ChainPoint
    , -- | implementable
      currentNodeEra ::
        m ()
    , -- | yes
      currentProtocolParameters ::
        m ProtocolParams
    , -- | yes, redundant
      currentNodeProtocolParameters ::
        m ProtocolParams
    , -- | implementable
      currentSlottingParameters ::
        m ()
    , -- | implementable using __polling__.
      watchNodeTip ::
        (ChainPoint -> m ()) ->
        m ()
    , -- | implementable. Converts slot numbers to wall clock time.
      timeInterpreter ::
        (
        )
    , -- | __no__, but fortunately only important for UX.
      -- We can track the number of addresses
      -- discovered so far, but we cannot know the total number of
      -- addresses associated with a wallet in advance.
      syncProgress ::
        ChainPoint ->
        m SyncProgress
    }

mkNetworkLayerBlockfrost :: NetworkLayer BlockfrostClient
mkNetworkLayerBlockfrost = nl
  where
    nl =
        NetworkLayer
            { chainSync = Light.ReadBlocks.lightSync
            , stakeDistribution = undefined
            , postTx = void . BF.submitTx
            , getCachedRewardAccountBalance =
                fetchRewardAccountBalances nl
            , fetchRewardAccountBalances =
                fmap BF._accountInfoRewardsSum . BF.getAccount
            , currentNodeEra = pure ()
            , currentNetworkTip = fromBlock <$> BF.getLatestBlock
            , currentProtocolParameters = BF.getLatestEpochProtocolParams
            , currentNodeProtocolParameters = currentProtocolParameters nl
            , currentSlottingParameters = pure ()
            , watchNodeTip = \_ -> pure ()
            , timeInterpreter = ()
            , syncProgress = \_ -> pure 0
            }

{-----------------------------------------------------------------------------
    ProtocolParameters
------------------------------------------------------------------------------}

{- | Protocol parameters.

 Some types have been changed or removed for clarity.
-}
data ProtocolParameters = ProtocolParameters
    { -- | yes, '_protocolParamsDecentralisationParam'
      decentralizationLevel ::
        (
        )
    , -- | yes, '_protocolParamsMaxTxSize', …
      txParameters ::
        (
        )
    , -- | yes, '_protocolParamsNOpt'
      desiredNumberOfStakePools ::
        Integer
    , -- | yes, '_protocolParamsMinUtxo'
      minimumUTxOvalue ::
        Lovelaces
    , -- | yes, '_protocolParamsKeyDeposit'
      stakeKeyDeposit ::
        Lovelaces
    , -- | implementable, '_protocolParamsEpoch'
      eras ::
        Int
    , -- | yes, '_protocolParamsMaxCollateralInputs'
      maximumCollateralInputCount ::
        Integer
    , -- | yes, '_protocolParamsCollateralPercent'
      minimumCollateralPercentage ::
        Integer
    , -- | implementable, '_protocolParamsPriceStep', '_protocolParamsPriceMem'
      executionUnitPrices ::
        Maybe Double
    }
