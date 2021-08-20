{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.CoinSelection where

performSelection
    :: forall m. (HasCallStack, MonadRandom m)
    -> SelectionConstraints
    -> SelectionData
    -> m (Either SelectionError (SelectionResult TokenBundle))
performSelection = undefined

data SelectionConstraints = SelectionConstraints
    { assessTokenBundleSize
        :: TokenBundleSizeAssessor
        -- ^ A function to assess the size of a token bundle. See the
        -- documentation for 'TokenBundleSizeAssessor' to learn about the
        -- expected properties of this function.
    , computeMinimumAdaQuantity
        :: TokenMap -> Coin
        -- ^ A function to compute the minimum ada quantity required by the
        -- protocol for a particular output.
    , computeMinimumCost
        :: SelectionSkeleton -> Coin
        -- ^ A function to compute the cost of a given selection. This function
        -- must not depend on the magnitudes of individual asset quantities
        -- held within each change output.
    , selectionLimit
        :: SelectionLimit
        -- ^ Specifies a limit to be adhered to when performing selection.
    }

data SelectionData = SelectionData
    { assetsToMint
        :: !TokenMap
        -- ^ Assets to mint: these provide input value to a transaction.
        --
        -- By minting tokens, we generally decrease the burden of the selection
        -- algorithm, allowing it to select fewer UTxO entries in order to
        -- cover the required outputs.
    , assetsToBurn
        :: !TokenMap
        -- ^ Assets to burn: these consume output value from a transaction.
        --
        -- By burning tokens, we generally increase the burden of the selection
        -- algorithm, requiring it to select more UTxO entries in order to
        -- cover the burn.
    , extraCoinSource
        :: !(Maybe Coin)
        -- ^ An optional extra source of ada.
    , outputsToCover
        :: !(NonEmpty TxOut)
        -- ^ The complete set of outputs to be covered.
    , utxoAvailable
        :: !UTxOIndex
        -- ^ A UTxO set from which inputs can be selected.
    }
    deriving (Eq, Generic, Show)

data SelectionResult change = SelectionResult
    { collateral
        :: ![(TxIn, TxOut)]
        -- ^ A list of collateral inputs selected from 'utxoAvailable'.
    , inputs
        :: !(NonEmpty (TxIn, TxOut))
        -- ^ A list of ordinary inputs selected from 'utxoAvailable'.
    , extraCoinSource
        :: !(Maybe Coin)
        -- ^ An optional extra source of ada.
    , outputsCovered
        :: ![TxOut]
    , changeGenerated
        :: ![change]
        -- ^ A list of generated change outputs.
    , utxoRemaining
        :: !UTxOIndex
        -- ^ The subset of 'utxoAvailable' that remains after performing
        -- the selection.
    }
    deriving (Generic, Eq, Show)

-- | A skeleton selection that can be used to estimate the cost of a final
--   selection.
--
-- Change outputs are deliberately stripped of their asset quantities, as the
-- fee estimation function must be agnostic to the magnitudes of these
-- quantities.
--
-- Increasing or decreasing the quantity of a particular asset in a change
-- output must not change the estimated cost of a selection.
--
data SelectionSkeleton = SelectionSkeleton
    { skeletonInputCount
        :: !Int
    , skeletonOutputs
        :: ![TxOut]
    , skeletonChange
        :: ![Set AssetId]
    }
    deriving (Eq, Generic, Show)
