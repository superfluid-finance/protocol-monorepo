{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Money.Systems.Superfluid.Instances.Simple.Types
    ( module Money.Systems.Superfluid.Concepts
    -- Wad
    , Wad (..)
    , toWad
    , wad4humanN
    , wad4human
    -- Timestamp
    , SimpleTimestamp (..)
    -- RealTimeBalance
    , SimpleRealTimeBalanceF (..)
    , SimpleRealTimeBalance
    , untappedValueL
    , mintedValueL
    , depositValueL
    -- SuperfluidTypes
    , SimpleSuperfluidTypes
    -- Agreements
    , SimpleMinterMonetaryUnitData
    , SimpleITAMonetaryUnitData
    , SimpleCFAOperation
    , SimpleCFAContractData
    , SimpleCFAMonetaryUnitData
    , SimpleDFAMonetaryUnitData
    , SimpleDFAContractData
    , SimpleDFAOperation
    , AnySimpleMonetaryUnitData (..)
    -- Indexes
    , SimpleUniversalData
    , SimplePublisherData
    , SimpleSubscriberData
    , SimpleDistributionContract
    , SimpleSubscriptionContract
    , ProportionalDistributionIndexID
    ) where

import           Control.Applicative                                                       (Applicative (..))
import           Data.Binary
import           Data.Default
import           Data.Foldable                                                             (toList)
import           Data.List                                                                 (intercalate)
import           Data.Proxy
import           Data.Type.TaggedTypeable
import           GHC.Generics                                                              (Generic)
import           Lens.Internal
import           Text.Printf                                                               (printf)


import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow         as CFMUD
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.DecayingFlow         as DFMUD
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantValue         as IVMUD
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.MintedValue          as MVMUD
--

import qualified Money.Systems.Superfluid.Agreements.ProportionalDistributionCommon        as PDCOMMON

import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement                 as CFA
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowDistributionAgreement     as CFDA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement                 as DFA
import qualified Money.Systems.Superfluid.Agreements.InstantDistributionAgreement          as IDA
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement              as ITA
import qualified Money.Systems.Superfluid.Agreements.MinterAgreement                       as MINTA
--
import qualified Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex as PDIDX
import qualified Money.Systems.Superfluid.Agreements.Indexes.UniversalIndex                as UIDX
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency                   as BBS

-- =====================================================================================================================
-- Value Type:
--   * 18 decimal digit fixed-precision integer
--   * an instance of Value

newtype Wad = Wad Integer
    deriving newtype (Default, Eq, Enum, Real, Ord, Num, Integral, Binary, Value)

toWad :: (RealFrac a) => a -> Wad
toWad x = Wad (round $ x * (10 ^ (18::Integer)))

wad4humanN :: Wad -> Integer -> String -- TODO use Nat?
wad4humanN (Wad wad) n
    | n >= 0 && n <= 18 = printf
        ("%0." ++ show n ++ "f")
        ((fromIntegral wad / (10 ^ (18::Integer))) :: Double)
    | otherwise = error "Invalid parameter"

wad4human :: Wad -> String
wad4human wad = wad4humanN wad 4

instance Show Wad where
    show = wad4human

-- =====================================================================================================================
-- * Type Values:

instance Show (UntappedValue Wad) where
    show (UntappedValue val) = show val ++ "@_"

instance TypedValueTag vtag => Show (TappedValue vtag Wad) where
    show (TappedValue val) = show val ++ "@" ++ tappedValueTag (Proxy @vtag)

instance Show (AnyTappedValue Wad) where
    show (AnyTappedValue (vtagProxy, val)) = show val ++ "@" ++ tappedValueTag vtagProxy

-- =====================================================================================================================
-- * Timestamp type

-- | Simple timestamp Type.
newtype SimpleTimestamp = SimpleTimestamp Int
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Binary, Timestamp)

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t ++ "s"

-- =====================================================================================================================
-- * RealTimeBalance Type

-- | Simple Real Time Balance Type.
data SimpleRealTimeBalanceF a = SimpleRealTimeBalanceF
    { untappedValue :: a
    , mintedValue   :: a
    , depositValue  :: a
    }
    deriving stock (Generic, Functor, Foldable, Traversable, Eq)

type SimpleRealTimeBalance = SimpleRealTimeBalanceF Wad

-- ** Lenses of RTB

untappedValueL :: Lens' SimpleRealTimeBalance Wad
untappedValueL  = $(field 'untappedValue)

mintedValueL   :: Lens' SimpleRealTimeBalance Wad
mintedValueL    = $(field 'mintedValue)

depositValueL  :: Lens' SimpleRealTimeBalance Wad
depositValueL   = $(field 'depositValue)

-- ** Class instances of RTB

instance Applicative SimpleRealTimeBalanceF where
    pure a = SimpleRealTimeBalanceF a a a
    liftA2 f (SimpleRealTimeBalanceF a b c) (SimpleRealTimeBalanceF a' b' c') =
        SimpleRealTimeBalanceF (f a a') (f b b') (f c c')

instance Show (SimpleRealTimeBalanceF Wad) where
    show rtb =
        (show       . netValueOfRTB      $ rtb) ++ " " ++
        (showDetail . typedValuesFromRTB $ rtb)
        where
        showDetail :: (UntappedValue Wad, [AnyTappedValue Wad]) -> String
        showDetail (UntappedValue uval, tvec) = "( "
            ++ show uval
            -- skip zero/default values
            ++ foldl ((++) . (++ ", ")) "" (map show . filter ((/= def) . untypeAnyTappedValue ) $ tvec)
            ++ " )"

instance Semigroup (SimpleRealTimeBalanceF Wad) where
    (<>) = liftA2 (+)

instance Monoid (SimpleRealTimeBalanceF Wad) where
    mempty = pure 0

instance RealTimeBalance SimpleRealTimeBalanceF Wad where
    valueToRTB uval = SimpleRealTimeBalanceF uval def def

    typedValuesToRTB (UntappedValue uval) tvec =
        SimpleRealTimeBalanceF uval def def <> foldMap g tvec
        -- extra correctly typed RTB monoid
        where g (AnyTappedValue (p, v)) = case typeRep p of
                  t | t == typeRep MVMUD.mintedValueTag -> SimpleRealTimeBalanceF def   v def
                    | t == typeRep BBS.bufferValueTag  -> SimpleRealTimeBalanceF def def   v
                    | otherwise -> error "Invalid monetary value tag"

    typedValuesFromRTB rtb = (UntappedValue (untappedValue rtb),
                              [ mkAnyTappedValue $ MVMUD.mkMintedValue $ mintedValue rtb
                              , mkAnyTappedValue $ BBS.mkBufferValue   $ depositValue rtb
                              ])

-- =====================================================================================================================
-- SuperfluidTypes Type

data SimpleSuperfluidTypes

instance SFTFloat Double

instance SuperfluidTypes SimpleSuperfluidTypes where
    type SFT_FLOAT SimpleSuperfluidTypes = Double
    type SFT_MVAL  SimpleSuperfluidTypes = Wad
    type SFT_TS    SimpleSuperfluidTypes = SimpleTimestamp
    type SFT_RTB_F SimpleSuperfluidTypes = SimpleRealTimeBalanceF
    dfa_default_lambda _ = log 2 / (3600 * 24 * 7)

-- =====================================================================================================================
-- Agreements
--

-- * MINTA
--

type SimpleMinterMonetaryUnitData = MINTA.MonetaryUnitData SimpleSuperfluidTypes

instance TaggedTypeable SimpleMinterMonetaryUnitData where
    tagFromProxy _ = "Minter"

instance Show SimpleMinterMonetaryUnitData where
    show (MVMUD.MkMonetaryUnitData x) = printf "{ uval = %s, mval = %s }"
        (show $ x^.MVMUD.untappedValue)
        (show $ x^.MVMUD.mintedValue)

-- * ITA
--

type SimpleITAContractData = ITA.ContractData SimpleSuperfluidTypes

instance TaggedTypeable SimpleITAContractData where
    tagFromProxy _ = "ITA#"

type SimpleITAMonetaryUnitData = ITA.MonetaryUnitData SimpleSuperfluidTypes

instance TaggedTypeable SimpleITAMonetaryUnitData where
    tagFromProxy _ = "ITA"

instance Show SimpleITAMonetaryUnitData where
    show (IVMUD.MkMonetaryUnitData x) = printf "{ uval = %s }"
        (show $ x^.IVMUD.untappedValue)

-- * CFA
--

type SimpleCFAMonetaryUnitData = CFA.MonetaryUnitData SimpleSuperfluidTypes

instance TaggedTypeable SimpleCFAMonetaryUnitData where
    tagFromProxy _ = "CFA"

instance Show SimpleCFAMonetaryUnitData where
    show (CFMUD.MkMonetaryUnitData x) = printf "{ t = %s, sval = %s, fr = %s }"
        (show $ x^.CFMUD.settledAt)
        (show $ x^.CFMUD.settledValue)
        (show $ x^.CFMUD.netFlowRate)

instance TaggedTypeable SimpleCFAContractData where
    tagFromProxy _ = "CFA#"

instance Show SimpleCFAContractData where
    show acd = printf "{ t_u = %s, fr = %s }"
        (show $ CFA.flow_updated_at acd)
        (show $ CFA.flow_rate acd)

type SimpleCFAContractData = CFA.ContractData SimpleSuperfluidTypes
type SimpleCFAOperation = CFA.Operation SimpleSuperfluidTypes

-- * DFA
--

type SimpleDFAMonetaryUnitData = DFA.MonetaryUnitData SimpleSuperfluidTypes

instance TaggedTypeable SimpleDFAMonetaryUnitData where
    tagFromProxy _ = "DFA"

instance Show SimpleDFAMonetaryUnitData where
    show (DFMUD.MkMonetaryUnitData x) = printf "{ λ = %s, t = %s, α = %s, ε = %s, buf = %s }"
        (show $ x^.DFMUD.decayingFactor)
        (show $ x^.DFMUD.settledAt)
        (show $ x^.DFMUD.αVal)
        (show $ x^.DFMUD.εVal)
        (show $ x^.DFMUD.settledBuffer)

instance TaggedTypeable SimpleDFAContractData where
    tagFromProxy _ = "DFA#"

type SimpleDFAContractData = DFA.ContractData SimpleSuperfluidTypes
type SimpleDFAOperation = DFA.Operation SimpleSuperfluidTypes

instance Show SimpleDFAContractData where
    show acd = printf "{ t_u = %s, δ = %s }"
        (show $ DFA.flow_last_updated_at acd)
        (show $ DFA.distribution_limit acd)

-- * IDA

instance TaggedTypeable (IDA.PublisherMonetaryUnitData SimpleSuperfluidTypes) where
    tagFromProxy _ = "IDA(P)"

instance Show (IDA.PublisherMonetaryUnitData SimpleSuperfluidTypes) where
    show (IVMUD.MkMonetaryUnitData x) = printf "pub { uval = %s }"
        (show $ x^.IVMUD.untappedValue)

instance TaggedTypeable (IDA.SubscriberMonetaryUnitData SimpleSuperfluidTypes) where
    tagFromProxy _ = "IDA(S)"

instance Show (IDA.SubscriberMonetaryUnitData SimpleSuperfluidTypes) where
    show (IVMUD.MkMonetaryUnitData x) = printf "sub { uval = %s }"
        (show $ x^.IVMUD.untappedValue)

-- * CFDA

instance TaggedTypeable (CFDA.PublisherMonetaryUnitData SimpleSuperfluidTypes) where
    tagFromProxy _ = "CFDA(P)"

instance Show (CFDA.PublisherMonetaryUnitData SimpleSuperfluidTypes) where
    show (CFMUD.MkMonetaryUnitData x) = printf "pub { t = %s, sval = %s, fr = %s }"
        (show $ x^.CFMUD.settledAt)
        (show $ x^.CFMUD.settledValue)
        (show $ x^.CFMUD.netFlowRate)

instance TaggedTypeable (CFDA.SubscriberMonetaryUnitData SimpleSuperfluidTypes) where
    tagFromProxy _ = "CFDA(S)"

instance Show (CFDA.SubscriberMonetaryUnitData SimpleSuperfluidTypes) where
    show (CFMUD.MkMonetaryUnitData x) = printf "sub { t = %s, sval = %s, fr = %s }"
        (show $ x^.CFMUD.settledAt)
        (show $ x^.CFMUD.settledValue)
        (show $ x^.CFMUD.netFlowRate)

-- * UIDX

type SimpleUniversalData = UIDX.UniversalData SimpleSuperfluidTypes

-- * PDIDX

type ProportionalDistributionIndexID = Int
type SimpleDistributionContract = PDIDX.DistributionContract SimpleSuperfluidTypes
type SimpleSubscriptionContract = PDIDX.SubscriptionContract SimpleSuperfluidTypes

instance TaggedTypeable (PDIDX.SubscriptionContract SimpleSuperfluidTypes) where
    tagFromProxy _ = "PD(S)#"
instance TaggedTypeable (PDIDX.DistributionContract SimpleSuperfluidTypes) where
    tagFromProxy _ = "PD(P)#"

deriving instance Show (PDCOMMON.DistributionContractBase SimpleSuperfluidTypes)
deriving instance Show (PDCOMMON.SubscriptionContractBase SimpleSuperfluidTypes)
deriving instance Show (IDA.DistributionContract SimpleSuperfluidTypes)
deriving instance Show (IDA.SubscriptionContract SimpleSuperfluidTypes)
deriving instance Show (CFDA.DistributionContract SimpleSuperfluidTypes)
deriving instance Show (CFDA.SubscriptionContract SimpleSuperfluidTypes)
deriving instance Show (PDIDX.DistributionContract SimpleSuperfluidTypes)
deriving instance Show (PDIDX.SubscriptionContract SimpleSuperfluidTypes)

type SimplePublisherData = PDIDX.PublisherData SimpleSuperfluidTypes
type SimpleSubscriberData = PDIDX.SubscriberData SimpleSuperfluidTypes


-- * AnyX.
--

-- | AnyMonetaryUnitData type.
data AnySimpleMonetaryUnitData = forall amud.
    ( MonetaryUnitDataClass amud SimpleSuperfluidTypes
    , TaggedTypeable amud
    , Show amud
    ) => MkSimpleMonetaryUnitData amud

instance Show AnySimpleMonetaryUnitData where
    show (MkSimpleMonetaryUnitData a) = show a

-- * Some useful AgreementOperationResultF instances
--

instance ( Foldable (AgreementOperationResultF ao)
         , Eq elem
         ) => Eq (AgreementOperationResultF ao elem) where
    a == b = and $ zipWith (==) (toList a) (toList b)

instance ( Foldable (AgreementOperationResultF ao)
         , Ord elem
         ) => Ord (AgreementOperationResultF ao elem) where
    a <= b = and $ zipWith (<=) (toList a) (toList b)

instance ( Foldable (AgreementOperationResultF ao)
         , Show elem
         ) => Show (AgreementOperationResultF ao elem) where
    show elems = "(" ++ (elems & toList & map show & intercalate ", ") ++ ")"
