{-# LANGUAGE TypeFamilies #-}
import           Money.Theory.SemanticMoney

newtype TestTime = TestTime Integer deriving (Enum, Eq, Ord, Num, Real, Integral, Show)
newtype TestMValue = TestMValue Integer deriving (Enum, Eq, Ord, Num, Real, Integral, Show)
newtype TestMUnit = TestMUnit Integer deriving (Enum, Eq, Ord, Num, Real, Integral, Show)
data TestMonetaryTypes
instance MonetaryTypes TestMonetaryTypes where
    type MT_TIME  TestMonetaryTypes = TestTime
    type MT_VALUE TestMonetaryTypes = TestMValue
    type MT_UNIT  TestMonetaryTypes = TestMUnit
deriving instance Show (BasicParticle TestMonetaryTypes)
type TesBasicParticle = BasicParticle TestMonetaryTypes
type TestUniversalIndex = UniversalIndex TestMonetaryTypes TesBasicParticle
type TestPDPoolIndex = PDPoolIndex TestMonetaryTypes TesBasicParticle
type TestPDPoolMember = PDPoolMember TestMonetaryTypes TesBasicParticle
type TestPDPoolMemberMU = PDPoolMemberMU TestMonetaryTypes TesBasicParticle

foreign export ccall uu_flow2 :: Int -> IO Int
uu_flow2 x = pure $ x + 42

main = pure ()
