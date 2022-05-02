module Cardano.Wallet.Primitive.Types.RewardAccount.Gen
    ( coarbitraryRewardAccount
    , genRewardAccount
    , shrinkRewardAccount
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Test.QuickCheck
    ( Gen, coarbitrary, vector )

import qualified Data.ByteString as BS

--------------------------------------------------------------------------------
-- Reward accounts generated according to the size parameter
--------------------------------------------------------------------------------

coarbitraryRewardAccount :: RewardAccount -> Gen a -> Gen a
coarbitraryRewardAccount = coarbitrary . BS.unpack . unRewardAccount

genRewardAccount :: Gen RewardAccount
genRewardAccount = RewardAccount . BS.pack <$> vector 28

shrinkRewardAccount :: RewardAccount -> [RewardAccount]
shrinkRewardAccount = const []
