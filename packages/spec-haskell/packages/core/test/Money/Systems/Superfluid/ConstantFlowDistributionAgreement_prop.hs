{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Money.Systems.Superfluid.ConstantFlowDistributionAgreement_prop (tests) where

import           Lens.Internal
import           Test.Hspec
import           Test.QuickCheck

import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow as CFMUD
import           Money.Systems.Superfluid.SystemTypes
import           Money.Systems.Superfluid.TestTypes

-- * Helpers

sameAs :: T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> Bool
(CFMUD.MkMonetaryUnitData a) `sameAs` (CFMUD.MkMonetaryUnitData b) =
    a^.CFMUD.settledValue == b^.CFMUD.settledValue &&
    a^.CFMUD.netFlowRate  == b^.CFMUD.netFlowRate

-- * Semigroup Monetary Unit Data Laws

semigroup_pubmud_associativity :: T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> Bool
semigroup_pubmud_associativity a b c = ((a <> b) <> c) `sameAs` (a <> (b <> c))

semigroup_pubmud_settles_pi :: T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> T_Timestamp -> Bool
    = mud_prop_semigroup_settles_pi

-- * Agreement Laws

-- ao_zero_sum_balance :: T_Timestamp -> NonEmptyList (T_CFDASubscriberOperation, T_Timestamp) -> Bool
-- ao_zero_sum_balance t aos = ao_prop_zero_sum_balance_series_ops t (getNonEmpty aos)

tests = describe "ConstantFlowDistributionAgreement properties" $ do
    it "CFDA semigroup Publisher MUD associativity"          $ property semigroup_pubmud_associativity
    it "CFDA semigroup Publisher MUD settles pi"             $ property semigroup_pubmud_settles_pi
    -- it "CFDA operations produces zero balance sum" $ property ao_zero_sum_balance
