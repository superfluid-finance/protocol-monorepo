{-# LANGUAGE TypeFamilies #-}

module Money.Theory.TestMonetaryTypes where

import           Test.QuickCheck

import           Money.Theory.SemanticMoney


newtype TestTime = TestTime Integer deriving (Enum, Eq, Ord, Num, Real, Integral, Show)
instance Arbitrary TestTime where
    arbitrary = TestTime . getNonNegative <$> arbitrary

newtype TestMValue = TestMValue Integer deriving (Enum, Eq, Ord, Num, Real, Integral, Show)
instance Arbitrary TestMValue where
    arbitrary = TestMValue <$> arbitrary

newtype TestMUnit = TestMUnit Integer deriving (Enum, Eq, Ord, Num, Real, Integral, Show)
instance Arbitrary TestMUnit where
    arbitrary = TestMUnit . getNonNegative <$> arbitrary

data TestMonetaryTypes
instance MonetaryTypes TestMonetaryTypes where
    type MT_TIME  TestMonetaryTypes = TestTime
    type MT_VALUE TestMonetaryTypes = TestMValue
    type MT_UNIT  TestMonetaryTypes = TestMUnit

deriving instance Show (BasicParticle TestMonetaryTypes)

type TesBasicParticle = BasicParticle TestMonetaryTypes
instance Arbitrary TesBasicParticle where
    arbitrary = BasicParticle <$> arbitrary <*> arbitrary <*> arbitrary

type TestUniversalIndex = UniversalIndex TestMonetaryTypes TesBasicParticle
deriving instance Show TestUniversalIndex
instance Arbitrary TestUniversalIndex where
    arbitrary = UniversalIndex <$> arbitrary

type TestPDPoolIndex = PDPoolIndex TestMonetaryTypes TesBasicParticle
deriving instance Show TestPDPoolIndex

type TestPDPoolMember = PDPoolMember TestMonetaryTypes TesBasicParticle
deriving instance Show TestPDPoolMember

type TestPDPoolMemberMU = PDPoolMemberMU TestMonetaryTypes TesBasicParticle
