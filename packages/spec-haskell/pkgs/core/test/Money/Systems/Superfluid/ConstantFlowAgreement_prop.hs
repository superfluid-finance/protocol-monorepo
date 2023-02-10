{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Money.Systems.Superfluid.ConstantFlowAgreement_prop (tests) where

import           Data.Default
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

data TestOperations = Nop () | Op T_CFAOperation deriving Show

instance Arbitrary TestOperations where
    arbitrary = oneof [ (arbitrary :: Gen T_CFAOperation) <&> Op
                      , (arbitrary :: Gen ()) <&> Nop
                      ]

ao_zero_sum_balance_2ps :: T_Timestamp -> NonEmptyList (TestOperations, T_Timestamp) -> Bool
ao_zero_sum_balance_2ps t0 aos0 = go (getNonEmpty aos0) t0 (def, def)
    where go ((Nop (), tΔ):aos) t cs =
              ao_sum_contract_balance p (MkAnyAgreementContractState cs) t' == mempty && go aos t' cs
              where t' = t + tΔ
          go ((Op ao, tΔ):aos) t cs =
              let cs' = ao_go_single_op cs ao t'
              in  ao_sum_contract_balance p (MkAnyAgreementContractState cs)  t' == mempty &&
                  ao_sum_contract_balance p (MkAnyAgreementContractState cs') t' == mempty &&
                  go aos t' cs'
              where t' = t + tΔ
          go [] _ _ = True
          p = Proxy @T_AnySemigroupMonetaryUnitData

tests = describe "ConstantFlowAgreement properties" $ do
    it "CFA semigroup MUD associativity"                              $ property semigroup_mud_associativity
    it "CFA semigroup MUD settles pi"                                 $ property semigroup_mud_settles_pi
    it "CFA operations produces zero balance sum between two parties" $ property ao_zero_sum_balance_2ps
