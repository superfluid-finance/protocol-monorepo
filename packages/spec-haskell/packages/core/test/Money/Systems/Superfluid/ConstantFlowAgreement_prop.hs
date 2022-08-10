{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Money.Systems.Superfluid.ConstantFlowAgreement_prop (tests) where

import           Lens.Internal
import           Test.Hspec
import           Test.QuickCheck

import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow as CFMUD
import           Money.Systems.Superfluid.Concepts
import           Money.Systems.Superfluid.TestTypes

-- * Helpers

sameAs :: TestCFAMonetaryUnitData -> TestCFAMonetaryUnitData -> Bool
(CFMUD.MkMonetaryUnitData a) `sameAs` (CFMUD.MkMonetaryUnitData b) =
    a^.CFMUD.settledValue == b^.CFMUD.settledValue &&
    a^.CFMUD.netFlowRate == b^.CFMUD.netFlowRate

-- * Semigroup Laws

semigroup_associativity :: TestCFAMonetaryUnitData -> TestCFAMonetaryUnitData -> TestCFAMonetaryUnitData -> Bool
semigroup_associativity a b c = ((a <> b) <> c) `sameAs` (a <> (b <> c))

-- * AMUD Laws

amud_semigroup_settles_pi :: TestCFAMonetaryUnitData -> TestCFAMonetaryUnitData -> TestTimestamp -> Bool
amud_semigroup_settles_pi = amud_prop_semigroup_settles_pi

tests = describe "ConstantFlowAgreement properties" $ do
    it "semigroup associativity"                           $ property semigroup_associativity
    it "agreement monetary unit data semigroup settles pi" $ property amud_semigroup_settles_pi
