module Light.StakePools where

import Data.Map (
    Map,
 )
import Light.Types

-- | Summary of stake distribution and stake pools obtained from network
data StakePoolsSummary = StakePoolsSummary
    { -- | implementable, see 'RewardParams'
      rewardParams :: RewardParams
    , -- | implementable, See 'RewardInfoPool'
      pools :: Map PoolId RewardInfoPool
    }

-- | Global parameters used for computing rewards
data RewardParams = RewardParams
    { -- | yes, '_protocolParamsNOpt'
      nOpt :: Int
    , -- | yes, '_protocolParamsA0'
      a0 :: Rational
    , -- | implementable, '_epochInfoFees', '_accountInfoReservesSum'?
      r :: Coin
    , -- | yes, '_genesisMaxLovelaceSupply'
      totalStake :: Coin
    }

{- | Information need for the computation of rewards, such as the'
 stake currently delegated to a pool, or the pool cost and margin.'
-}
data RewardInfoPool = RewardInfoPool
    { -- | yes, '_poolStakeDistributionAmount'
      stakeRelative :: Rational
    , -- | yes, '_poolInfoDeclaredPledge'
      ownerPledge :: Coin
    , -- | yes (?), '_poolInfoLivePledge'
      ownerStake :: Coin
    , -- | yes, redundant
      ownerStakeRelative :: Rational
    , -- | yes, '_poolInfoFixedCost'
      cost :: Coin
    , -- | yes, '_poolInfoMarginCost'
      margin :: Rational
    , -- | implementable, 'getPoolHistory', 'PoolHistory', '_poolHistoryBlocks'
      performanceEstimate :: Double
    }
