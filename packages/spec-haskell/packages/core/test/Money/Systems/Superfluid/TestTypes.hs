{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.TestTypes where

import           Control.Applicative
import           Data.Coerce
import           Data.Default
import           Data.Typeable
import           GHC.Generics

import           Test.QuickCheck

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement         as CFA
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow as CFMUD
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.MintedValue  as MVMUD
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency           as BBS


-- * Timestamp

newtype TestTimestamp = TestTimestamp Int
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Timestamp, Show, Arbitrary)

-- * Value
newtype TestMVal = TestMVal Integer
    deriving (Default, Eq, Enum, Real, Ord, Num, Integral, Value, Show, Arbitrary)

deriving instance Show (UntappedValue TestMVal)

-- * RealTimeBalance
data TestRealTimeBalanceF a = TestRealTimeBalanceF
    { untappedValue :: a
    , mintedValue   :: a
    , depositValue  :: a
    }
    deriving stock (Generic, Functor, Foldable, Traversable, Show, Eq)
type TestRealTimeBalance = TestRealTimeBalanceF TestMVal

instance Applicative TestRealTimeBalanceF where
    pure a = TestRealTimeBalanceF a a a
    liftA2 f (TestRealTimeBalanceF a b c) (TestRealTimeBalanceF a' b' c') =
        TestRealTimeBalanceF (f a a') (f b b') (f c c')
instance Semigroup (TestRealTimeBalanceF TestMVal) where (<>) = liftA2 (+)
instance Monoid (TestRealTimeBalanceF TestMVal) where mempty = pure 0
-- instance Eq (TestRealTimeBalanceF TestMVal) where (==) = (==)

instance RealTimeBalance TestRealTimeBalanceF TestMVal where
    valueToRTB uval = TestRealTimeBalanceF uval def def

    typedValuesToRTB (UntappedValue uval) tvec =
        TestRealTimeBalanceF uval def def <> foldMap g tvec
        -- extra correctly typed RTB monoid
        where g (AnyTappedValue (p, v)) = case typeRep p of
                  t | t == typeRep MVMUD.mintedValueTag -> TestRealTimeBalanceF def   v def
                    | t == typeRep BBS.bufferValueTag  -> TestRealTimeBalanceF def def   v
                    | otherwise -> error "Invalid monetary value tag"

    typedValuesFromRTB rtb = (UntappedValue (untappedValue rtb),
                              [ mkAnyTappedValue $ MVMUD.mkMintedValue $ mintedValue rtb
                              , mkAnyTappedValue $ BBS.mkBufferValue   $ depositValue rtb
                              ])

instance Arbitrary TestRealTimeBalance where
    arbitrary = do
        u  <- arbitrary
        m  <- arbitrary
        d  <- arbitrary
        return TestRealTimeBalanceF
            { untappedValue = TestMVal u
            , mintedValue   = TestMVal m
            , depositValue  = TestMVal d
            }

-- * SuperfluidTypes Type

data TestSuperfluidTypes

instance SFTFloat Double

instance SuperfluidTypes TestSuperfluidTypes where
    type SFT_FLOAT TestSuperfluidTypes = Double
    type SFT_MVAL  TestSuperfluidTypes = TestMVal
    type SFT_TS    TestSuperfluidTypes = TestTimestamp
    type SFT_RTB_F TestSuperfluidTypes = TestRealTimeBalanceF
    dfa_default_lambda _ = log 2 / (3600 * 24 * 7)

-- * BBS
deriving instance Show (BBS.BufferValue TestMVal)

-- * CFA

type TestCFAMonetaryUnitData = CFA.MonetaryUnitData TestSuperfluidTypes

instance Arbitrary TestCFAMonetaryUnitData where
    arbitrary = do
        t_s <- arbitrary
        sv  <- arbitrary
        nfr <- arbitrary
        return $ CFMUD.MkMonetaryUnitData CFA.MonetaryUnitLenses
            { CFA.settled_at    = TestTimestamp t_s
            , CFA.settled_value = coerce $ TestMVal sv
            , CFA.net_flow_rate = TestMVal nfr
            }
deriving instance Show (CFA.MonetaryUnitLenses TestSuperfluidTypes)
deriving instance Show TestCFAMonetaryUnitData
