{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.TestTypes where

import           Control.Applicative
import           Data.Coerce
import           Data.Default
import           Data.Functor                                                                                   ((<&>))
import           Data.Type.Any
import           Data.Typeable
import           GHC.Generics
import           Math.Extras.Double
    ( Tolerance
    , fuzzyEq
    )

import           Test.QuickCheck

import           Money.Systems.Superfluid.SystemTypes
--
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistribution.ConstantFlowDistributionAgreement as CFDA
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex                              as PDIDX
import qualified Money.Systems.Superfluid.Agreements.Universal.ConstantFlowAgreement                            as CFA
import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow                                         as CFMUD
import qualified Money.Systems.Superfluid.MonetaryUnitData.MintedValue                                          as MVMUD
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency                                        as BBS


-- * Timestamp

newtype T_Timestamp = T_Timestamp Integer
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Timestamp, Show)

instance Arbitrary T_Timestamp where
    arbitrary = arbitrary <&> id <&> T_Timestamp

-- * Value

newtype T_MVal = T_MVal Integer
    deriving newtype (Default, Eq, Enum, Real, Ord, Num, Integral, MonetaryValue, Show, Arbitrary)

deriving instance Show (UntappedValue T_MVal)

mval_base :: T_MVal
mval_base = floor (1e18 :: Double)

flowrate_base :: T_MVal
flowrate_base = floor (fromIntegral mval_base / (3600 * 24 * 30) :: Double) -- 1$ / month

test_flowrate_per_mon_max :: Int
test_flowrate_per_mon_max = floor (100e6 :: Double)

fuzzy_tolerance :: Tolerance
fuzzy_tolerance = fromIntegral flowrate_base / 1e4 -- accurate to 4 decimals of $1

fuzzyEqMVal :: T_MVal -> T_MVal -> Bool
fuzzyEqMVal a b = fuzzyEq fuzzy_tolerance (fromIntegral a) (fromIntegral b)

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

instance RealTimeBalance T_RealTimeBalanceF T_MVal where
    valueToRTB _ uval = T_RealTimeBalanceF uval def def

    typedValuesToRTB = foldMap g
        where g (AnyTypedValue (p, v)) =
                  let v' = coerce v
                  in case typeRep p of
                      t | t == typeRep (Proxy @(UntappedValue T_MVal))     -> T_RealTimeBalanceF  v' def def
                        | t == typeRep (Proxy @(MVMUD.MintedValue T_MVal)) -> T_RealTimeBalanceF def  v' def
                        | t == typeRep (Proxy @(BBS.BufferValue T_MVal))   -> T_RealTimeBalanceF def def  v'
                        | otherwise -> error ("Invalid value tag: " <> show t)

    typedValuesFromRTB rtb = [ mkAnyTypedValue $ MkUntappedValue     $ untappedValue rtb
                             , mkAnyTypedValue $ MVMUD.MkMintedValue $ mintedValue rtb
                             , mkAnyTypedValue $ BBS.MkBufferValue   $ depositValue rtb
                             ]

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

-- * Superfluid Core Types

data T_SuperfluidSystem

instance SFTFloat Double

instance SuperfluidCoreTypes T_SuperfluidSystem where
    type SFT_FLOAT T_SuperfluidSystem = Double
    type SFT_MVAL  T_SuperfluidSystem = T_MVal
    type SFT_TS    T_SuperfluidSystem = T_Timestamp
    type SFT_RTB_F T_SuperfluidSystem = T_RealTimeBalanceF

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
            { CFA.settled_at    = t_s
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

-- * PDIDX

type T_PDIDXSubscriberContract = PDIDX.SubscriberContract T_SuperfluidSystem
type T_PDIDXSubscriberOperation = AgreementOperation T_PDIDXSubscriberContract

instance Arbitrary T_PDIDXSubscriberOperation where
    arbitrary = chooseBoundedIntegral (0 :: Int, 1000)
        <&> fromIntegral <&> PDIDX.Subscribe

deriving instance Show T_PDIDXSubscriberOperation

-- * CFDA

type T_CFDAPublisherMUD = CFDA.PublisherMonetaryUnitData T_SuperfluidSystem

instance Arbitrary T_CFDAPublisherMUD where
    arbitrary = do
        t_s <- arbitrary
        sv  <- arbitrary
        tfr <- arbitrary
        return $ CFMUD.MkMonetaryUnitData CFDA.PublisherData
            { CFDA.pub_settled_at      = t_s
            , CFDA.pub_settled_value   = coerce $ T_MVal sv
            , CFDA.pub_total_flow_rate = T_MVal tfr
            }
deriving instance Show (CFDA.PublisherData T_SuperfluidSystem)
deriving instance Show T_CFDAPublisherMUD

type T_CFDAPublisherContract = CFDA.PublisherContract T_SuperfluidSystem
type T_CFDAPublisherOperation = AgreementOperation T_CFDAPublisherContract

instance Arbitrary T_CFDAPublisherOperation where
    arbitrary = chooseBoundedIntegral  (0 :: Int, test_flowrate_per_mon_max)
        <&> (* flowrate_base) . fromIntegral <&> CFDA.UpdateDistributionFlowRate
deriving instance Show T_CFDAPublisherOperation

type T_CFDASubscriberContract = CFDA.SubscriberContract T_SuperfluidSystem

-- * Superfluid System Types

-- | Existential type wrapper of monetary unit data
data T_AnyMonetaryUnitData = forall mud.
    MonetaryUnitDataClass mud T_SuperfluidSystem
    => MkAnyMonetaryUnitData mud

instance MonetaryUnitDataClass T_AnyMonetaryUnitData T_SuperfluidSystem where
    balanceProvided (MkAnyMonetaryUnitData a) = balanceProvided a

instance SuperfluidSystemTypes T_SuperfluidSystem where
    type SFT_ANY_MUD T_SuperfluidSystem = T_AnyMonetaryUnitData
    dfa_default_lambda _ = log 2 / (3600 * 24 * 7)

-- | Existential type wrapper of semigroup monetary unit data.
data T_AnySemigroupMonetaryUnitData = forall mud. SemigroupMonetaryUnitData mud T_SuperfluidSystem
    => MkAnySemigroupMonetaryUnitData mud
instance MonetaryUnitDataClass T_AnySemigroupMonetaryUnitData T_SuperfluidSystem where
    balanceProvided (MkAnySemigroupMonetaryUnitData a) = balanceProvided a

instance T_AnySemigroupMonetaryUnitData `IsAnyTypeOf` MPTC_Flip SemigroupMonetaryUnitData T_SuperfluidSystem where
    mkAny _ = MkAnySemigroupMonetaryUnitData
