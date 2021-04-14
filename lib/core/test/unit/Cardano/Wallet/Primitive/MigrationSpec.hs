{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.MigrationSpec
    where

import Prelude

import Cardano.Wallet.Primitive.Migration
    ( createPlan
    , MigrationPlan (..)
    )
import Data.Ratio
    ( (%) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Cardano.Wallet.Primitive.Migration.Selection
    ( Selection (..)
    )
import Cardano.Wallet.Primitive.Migration.SelectionSpec
    ( MockTxConstraints (..)
    , MockInputId
    , genMockTxConstraints
    , genMockInput
    , genCoinRange
    , unMockTxConstraints
    , counterexampleMap
    )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxConstraints (..), txOutputCoinCost )
import Control.Monad
    ( replicateM )
import Data.Quantity
    ( Percentage )
import Data.Set
    ( Set )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Property
    , Blind (..)
    , Gen
    , choose
    , Arbitrary (..)
    , oneof
    , conjoin
    , property
    , checkCoverage
    , cover
    , counterexample
    , (===)
    )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()

import qualified Cardano.Wallet.Primitive.Migration.Selection as Selection
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.MigrationSpec" $

    modifyMaxSuccess (const 10) $ do

    parallel $ describe "Creating migration plans" $ do

        it "prop_createPlan" $
            property prop_createPlan

--------------------------------------------------------------------------------
-- Creating migration plans
--------------------------------------------------------------------------------

data MockCreatePlanArguments = MockCreatePlanArguments
    { mockConstraints :: MockTxConstraints
    , mockInputs :: [(MockInputId, TokenBundle)]
    , mockRewardBalance :: Coin
    }
    deriving (Eq, Show)

instance Arbitrary MockCreatePlanArguments where
    arbitrary = genMockCreatePlanArguments

genMockCreatePlanArguments :: Gen MockCreatePlanArguments
genMockCreatePlanArguments = do
    mockConstraints <- genMockTxConstraints
    mockInputCount <- choose (0, 100)
    mockInputs <- replicateM mockInputCount genMockInput
    mockRewardBalance <- oneof
        [ pure (Coin 0)
        , genCoinRange (Coin 1) (Coin 1_000_000)
        ]
    pure MockCreatePlanArguments
        { mockConstraints
        , mockInputs
        , mockRewardBalance
        }

prop_createPlan :: Blind MockCreatePlanArguments -> Property
prop_createPlan mockArgs =
    checkCoverage $
    counterexample counterexampleText $
    cover 0 (percentageOfAdaNotSelected <= unsafeMkPercentage (5 % 100))
        "percentageOfAdaNotSelected <= 5 %" $
    cover 5 (selectionCount == 1)
        "selectionCount == 1" $
    cover 5 (selectionCount == 2)
        "selectionCount == 2" $
    cover 5 (selectionCount == 3)
        "selectionCount == 3" $
    conjoin
        [ inputIds === inputIdsSelected `Set.union` inputIdsNotSelected
        , totalFee result === totalFeeExpected
        , Map.elems erroneouslyUnselectedNonDustInputCoins === []
        ]
  where
    Blind MockCreatePlanArguments
        { mockConstraints
        , mockInputs
        , mockRewardBalance
        } = mockArgs
    constraints = unMockTxConstraints mockConstraints
    result = createPlan constraints mockInputs mockRewardBalance

    inputIds :: Set MockInputId
    inputIds = Set.fromList (fst <$> mockInputs)

    inputIdsSelected :: Set MockInputId
    inputIdsSelected = Set.fromList
        [ i
        | s <- selections result
        , (i, _) <- NE.toList (inputs s)
        ]

    inputIdsNotSelected :: Set MockInputId
    inputIdsNotSelected = Set.fromList (fst <$> unselected result)

    selectionCount = length (selections result)

    totalFeeExpected :: Coin
    totalFeeExpected = F.foldMap fee (selections result)

    -- we really want to get all the inputs which can make a transaction for
    -- themselves.
    nonDustInputCoins :: Map MockInputId Coin
    nonDustInputCoins =
        Map.fromList $ catMaybes $ toMatchingInputCoin <$> mockInputs
      where
        toMatchingInputCoin
            :: (MockInputId, TokenBundle) -> Maybe (MockInputId, Coin)
        toMatchingInputCoin (i, b)
            | Just c <- TokenBundle.toCoin b, match c =
                Just (i, c)
            | otherwise =
                Nothing
          where
            match :: Coin -> Bool
            match c = c >= mconcat
                [ txOutputMinimumAdaQuantity constraints mempty
                , txBaseCost constraints
                , txInputCost constraints
                , txOutputCoinCost constraints c
                ]

    unselectedNonDustInputCoins :: Map MockInputId Coin
    unselectedNonDustInputCoins =
        nonDustInputCoins `Map.restrictKeys` inputIdsNotSelected

    -- this can be erroneouslyUnselectedInputs
    -- perhaps we want to sort in decreasing order of ada quantity.
    erroneouslyUnselectedNonDustInputCoins :: Map MockInputId Coin
    erroneouslyUnselectedNonDustInputCoins =
        Map.filterWithKey condition unselectedNonDustInputCoins
      where
        condition i c =
            case selectionResult of
                Left _ ->
                    False
                Right _ ->
                    True
          where
            selectionResult = Selection.create constraints (Coin 0)
                ((i, TokenBundle.fromCoin c) :| [])

    totalAdaAvailable :: Coin
    totalAdaAvailable = F.foldMap (view #coin . snd) mockInputs

    totalAdaNotSelected :: Coin
    totalAdaNotSelected = F.foldMap (view #coin . snd) (unselected result)

    percentageOfAdaNotSelected :: Percentage
    percentageOfAdaNotSelected
        | totalAdaAvailable == Coin 0 =
            unsafeMkPercentage 0
        | otherwise =
            unsafeMkPercentage $ (%)
                (coinToInteger totalAdaNotSelected)
                (coinToInteger totalAdaAvailable)

    counterexampleText = counterexampleMap
        [ ( "mockConstraints"
          , show mockConstraints )
        , ( "length nonDustInputCoins"
          , show (length nonDustInputCoins) )
        ]

--------------------------------------------------------------------------------
-- Miscellaneous types and functions
--------------------------------------------------------------------------------

coinToInteger :: Coin -> Integer
coinToInteger = fromIntegral . unCoin
