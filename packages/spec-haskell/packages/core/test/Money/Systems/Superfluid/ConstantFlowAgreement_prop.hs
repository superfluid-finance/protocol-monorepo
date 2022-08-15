{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Money.Systems.Superfluid.ConstantFlowAgreement_prop (tests) where

import           Data.Proxy
import           Lens.Internal
import           Test.Hspec
import           Test.QuickCheck

import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow as CFMUD
import           Money.Systems.Superfluid.SystemTypes
import           Money.Systems.Superfluid.TestTypes

-- * Helpers

sameAs :: T_CFAMonetaryUnitData -> T_CFAMonetaryUnitData -> Bool
(CFMUD.MkMonetaryUnitData a) `sameAs` (CFMUD.MkMonetaryUnitData b) =
    a^.CFMUD.settledValue == b^.CFMUD.settledValue &&
    a^.CFMUD.netFlowRate  == b^.CFMUD.netFlowRate

-- * Semigroup Monetary Unit Data Laws

semigroup_mud_associativity :: T_CFAMonetaryUnitData -> T_CFAMonetaryUnitData -> T_CFAMonetaryUnitData -> Bool
semigroup_mud_associativity a b c = ((a <> b) <> c) `sameAs` (a <> (b <> c))

semigroup_mud_settles_pi :: T_CFAMonetaryUnitData -> T_CFAMonetaryUnitData -> T_Timestamp -> Bool
semigroup_mud_settles_pi = mud_prop_semigroup_settles_pi

-- * Agreement Laws

ao_zero_sum_balance :: T_Timestamp -> NonEmptyList (T_CFAOperation, T_Timestamp) -> Bool
ao_zero_sum_balance t aos = ao_prop_zero_sum_balance_series_ops p t (getNonEmpty aos)
    where p = Proxy @T_AnySemigroupMonetaryUnitData

tests = describe "ConstantFlowAgreement properties" $ do
    it "CFA semigroup MUD associativity"          $ property semigroup_mud_associativity
    it "CFA semigroup MUD settles pi"             $ property semigroup_mud_settles_pi
    it "CFA operations produces zero balance sum" $ property ao_zero_sum_balance
