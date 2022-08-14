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

import           Money.Systems.Superfluid.SystemTypes
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement             as CFA
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowDistributionAgreement as CFDA
import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow                as CFMUD
import qualified Money.Systems.Superfluid.MonetaryUnitData.MintedValue                 as MVMUD
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency               as BBS


-- * Timestamp

newtype T_Timestamp = T_Timestamp Int
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Timestamp, Show, Arbitrary)

-- * Value
newtype T_MVal = T_MVal Integer
    deriving (Default, Eq, Enum, Real, Ord, Num, Integral, Value, Show, Arbitrary)

deriving instance Show (UntappedValue T_MVal)

-- * RealTimeBalance
data T_RealTimeBalanceF a = T_RealTimeBalanceF
    { untappedValue :: a
    , mintedValue   :: a
    , depositValue  :: a
    }
    deriving stock (Generic, Functor, Foldable, Traversable, Show, Eq)
type T_RealTimeBalance = T_RealTimeBalanceF T_MVal

instance Applicative T_RealTimeBalanceF where
    pure a = T_RealTimeBalanceF a a a
    liftA2 f (T_RealTimeBalanceF a b c) (T_RealTimeBalanceF a' b' c') =
        T_RealTimeBalanceF (f a a') (f b b') (f c c')
instance Semigroup (T_RealTimeBalanceF T_MVal) where (<>) = liftA2 (+)
instance Monoid (T_RealTimeBalanceF T_MVal) where mempty = pure 0
-- instance Eq (T_RealTimeBalanceF T_MVal) where (==) = (==)

instance RealTimeBalance T_RealTimeBalanceF T_MVal where
    valueToRTB uval = T_RealTimeBalanceF uval def def

    typedValuesToRTB (UntappedValue uval) tvec =
        T_RealTimeBalanceF uval def def <> foldMap g tvec
        -- extra correctly typed RTB monoid
        where g (AnyTappedValue (p, v)) = case typeRep p of
                  t | t == typeRep MVMUD.mintedValueTag -> T_RealTimeBalanceF def   v def
                    | t == typeRep BBS.bufferValueTag  -> T_RealTimeBalanceF def def   v
                    | otherwise -> error "Invalid monetary value tag"

    typedValuesFromRTB rtb = (UntappedValue (untappedValue rtb),
                              [ mkAnyTappedValue $ MVMUD.mkMintedValue $ mintedValue rtb
                              , mkAnyTappedValue $ BBS.mkBufferValue   $ depositValue rtb
                              ])

instance Arbitrary T_RealTimeBalance where
    arbitrary = do
        u  <- arbitrary
        m  <- arbitrary
        d  <- arbitrary
        return T_RealTimeBalanceF
            { untappedValue = T_MVal u
            , mintedValue   = T_MVal m
            , depositValue  = T_MVal d
            }

-- * Superfluid System

data T_SuperfluidSystem

instance SFTFloat Double

instance SuperfluidCoreTypes T_SuperfluidSystem where
    type SFT_FLOAT T_SuperfluidSystem = Double
    type SFT_MVAL  T_SuperfluidSystem = T_MVal
    type SFT_TS    T_SuperfluidSystem = T_Timestamp
    type SFT_RTB_F T_SuperfluidSystem = T_RealTimeBalanceF

-- | Existential type wrapper of monetary unit data
data AnyMonetaryUnitData sft = forall mud. MonetaryUnitDataClass mud sft => MkAnyMonetaryUnitData mud
instance SuperfluidCoreTypes sft => MonetaryUnitDataClass (AnyMonetaryUnitData sft) sft where
    balanceProvided (MkAnyMonetaryUnitData a) = balanceProvided a

-- instance SuperfluidCoreTypes sft => AnyMonetaryUnitData sft `IsAnyTypeOf` MonetaryUnitDataClass where
--    mkAny :: (SuperfluidCoreTypes sft, MonetaryUnitDataClass sft e) =>
--        e -> AnyMonetaryUnitData sft
--    mkAny = MkAnyMonetaryUnitData

instance SuperfluidSystemTypes T_SuperfluidSystem where
    type SFT_ANY_MUD T_SuperfluidSystem = AnyMonetaryUnitData T_SuperfluidSystem
    dfa_default_lambda _ = log 2 / (3600 * 24 * 7)

-- * BBS
deriving instance Show (BBS.BufferValue T_MVal)

-- * CFA

type T_CFAMonetaryUnitData = CFA.MonetaryUnitData T_SuperfluidSystem

instance Arbitrary T_CFAMonetaryUnitData where
    arbitrary = do
        t_s <- arbitrary
        sv  <- arbitrary
        nfr <- arbitrary
        return $ CFMUD.MkMonetaryUnitData CFA.MonetaryUnitLenses
            { CFA.settled_at    = T_Timestamp t_s
            , CFA.settled_value = coerce $ T_MVal sv
            , CFA.net_flow_rate = T_MVal nfr
            }

deriving instance Show (CFA.MonetaryUnitLenses T_SuperfluidSystem)

type T_CFAContractData = CFA.ContractData T_SuperfluidSystem
type T_CFAOperation = AgreementOperation T_CFAContractData

instance Arbitrary T_CFAOperation where
    arbitrary = CFA.UpdateFlow <$> arbitrary
deriving instance Show T_CFAMonetaryUnitData
deriving instance Show T_CFAOperation

-- * CFDA

type T_CFDAPublisherMUD = CFDA.PublisherMonetaryUnitData T_SuperfluidSystem

instance Arbitrary T_CFDAPublisherMUD where
    arbitrary = do
        t_s <- arbitrary
        sv  <- arbitrary
        tfr <- arbitrary
        return $ CFMUD.MkMonetaryUnitData CFDA.PublisherData
            { CFDA.pub_settled_at      = T_Timestamp t_s
            , CFDA.pub_settled_value   = coerce $ T_MVal sv
            , CFDA.pub_total_flow_rate = T_MVal tfr
            }
deriving instance Show (CFDA.PublisherData T_SuperfluidSystem)
deriving instance Show T_CFDAPublisherMUD

type T_CFDAPublisherContract = CFDA.PublisherContract T_SuperfluidSystem
type T_CFDAPublisherOperation = AgreementOperation T_CFDAPublisherContract

type T_CFDASubscriberContract = CFDA.SubscriberContract T_SuperfluidSystem
type T_CFDASubscriberOperation = AgreementOperation T_CFDASubscriberContract
