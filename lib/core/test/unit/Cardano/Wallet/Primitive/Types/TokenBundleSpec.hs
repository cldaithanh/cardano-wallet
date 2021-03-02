{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.TokenBundleSpec
    ( spec
    ) where

import Prelude

import Cardano.Numeric.Util
    ( inAscendingPartialOrder )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantitySmallPositive, shrinkTokenQuantitySmallPositive )
import Data.Ratio
    ( (%) )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..), Property, property, (===), (==>) )
import Test.QuickCheck.Classes
    ( eqLaws, monoidLaws, semigroupLaws, semigroupMonoidLaws )
import Test.Utils.Laws
    ( testLawsMany )
import Test.Utils.Laws.PartialOrd
    ( partialOrdLaws )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Data.Foldable as F

spec :: Spec
spec =
    describe "Token bundle properties" $
    modifyMaxSuccess (const 1000) $ do

    describe "Class instances obey laws" $ do
        testLawsMany @TokenBundle
            [ eqLaws
            , monoidLaws
            , partialOrdLaws
            , semigroupLaws
            , semigroupMonoidLaws
            ]

    describe "Partitioning quantities with an upper bound" $ do

        it "prop_equipartitionQuantitiesWithUpperBound_length" $
            property prop_equipartitionQuantitiesWithUpperBound_length
        it "prop_equipartitionQuantitiesWithUpperBound_order" $
            property prop_equipartitionQuantitiesWithUpperBound_order
        it "prop_equipartitionQuantitiesWithUpperBound_sum" $
            property prop_equipartitionQuantitiesWithUpperBound_sum

--------------------------------------------------------------------------------
-- Partitioning quantities according to an upper bound
--------------------------------------------------------------------------------

-- | Computes the number of parts that 'equipartitionQuantitiesWithUpperBound'
--   should return.
--
equipartitionQuantitiesWithUpperBound_expectedLength
    :: TokenBundle -> TokenQuantity -> Int
equipartitionQuantitiesWithUpperBound_expectedLength
    (TokenBundle _ m) (TokenQuantity maxQuantity) =
        max 1 $ ceiling $ currentMaxQuantity % maxQuantity
  where
    TokenQuantity currentMaxQuantity = TokenMap.maximumQuantity m

prop_equipartitionQuantitiesWithUpperBound_length
    :: TokenBundle -> TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_length m maxQuantity =
    maxQuantity > TokenQuantity.zero ==>
        length (TokenBundle.equipartitionQuantitiesWithUpperBound m maxQuantity)
            === equipartitionQuantitiesWithUpperBound_expectedLength
                m maxQuantity

prop_equipartitionQuantitiesWithUpperBound_order
    :: TokenBundle -> TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_order m maxQuantity =
    maxQuantity > TokenQuantity.zero ==>
        inAscendingPartialOrder
            (TokenBundle.equipartitionQuantitiesWithUpperBound m maxQuantity)

prop_equipartitionQuantitiesWithUpperBound_sum
    :: TokenBundle -> TokenQuantity -> Property
prop_equipartitionQuantitiesWithUpperBound_sum m maxQuantity =
    maxQuantity > TokenQuantity.zero ==>
        F.fold (TokenBundle.equipartitionQuantitiesWithUpperBound m maxQuantity)
            === m

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange

instance Arbitrary TokenQuantity where
    arbitrary = genTokenQuantitySmallPositive
    shrink = shrinkTokenQuantitySmallPositive
