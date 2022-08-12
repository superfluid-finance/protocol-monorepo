{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Money.Systems.Superfluid.ConstantFlowDistributionAgreement_prop (tests) where

import           Lens.Internal
import           Test.Hspec
import           Test.QuickCheck

import           Money.Systems.Superfluid.Concepts
import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow as CFMUD
import           Money.Systems.Superfluid.TestTypes

-- * Helpers

sameAs :: T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> Bool
(CFMUD.MkMonetaryUnitData a) `sameAs` (CFMUD.MkMonetaryUnitData b) =
    a^.CFMUD.settledValue == b^.CFMUD.settledValue &&
    a^.CFMUD.netFlowRate  == b^.CFMUD.netFlowRate

-- * Semigroup Laws

semigroup_associativity :: T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> Bool
semigroup_associativity a b c = ((a <> b) <> c) `sameAs` (a <> (b <> c))

-- * AMUD Laws

mud_semigroup_settles_pi :: T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> T_Timestamp -> Bool
mud_semigroup_settles_pi = mud_prop_semigroup_settles_pi

tests = describe "ConstantFlowDistributionAgreement properties" $ do
    it "semigroup associativity" $ property semigroup_associativity
    it "semigroup settles pi"    $ property mud_semigroup_settles_pi
