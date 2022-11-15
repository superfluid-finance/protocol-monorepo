{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Money.Systems.Superfluid.Instances.Simple.Types
    ( module Money.Systems.Superfluid.SystemTypes
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
    , SimpleSuperfluidSystem
    -- Agreements
    , SimpleMinterMonetaryUnitData
    , SimpleITAMonetaryUnitData
    , SimpleCFAContractData
    , SimpleCFAMonetaryUnitData
    , SimpleDFAMonetaryUnitData
    , SimpleDFAContractData
    , AnySimpleMonetaryUnitData (..)
    -- Indexes
    , SimpleUniversalData
    , SimplePublisherData
    , SimpleSubscriberData
    , SimpleDistributionContract
    , SimpleSubscriptionContract
    , ProportionalDistributionIndexID
    ) where

import           Control.Applicative
    ( Applicative (..)
    )
import           Data.Binary
import           Data.Coerce
import           Data.Default
import           Data.Foldable                                                                                  (toList)
import           Data.List
    ( intercalate
    )
import           Data.Proxy
import           Data.Type.Any
import           Data.Type.TaggedTypeable
import           GHC.Generics
    ( Generic
    )
import           Lens.Internal
import           Text.Printf                                                                                    (printf)


import           Money.Systems.Superfluid.SystemTypes
--
import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow                                         as CFMUD
import qualified Money.Systems.Superfluid.MonetaryUnitData.DecayingFlow                                         as DFMUD
import qualified Money.Systems.Superfluid.MonetaryUnitData.InstantValue                                         as IVMUD
import qualified Money.Systems.Superfluid.MonetaryUnitData.MintedValue                                          as MVMUD
--

import qualified Money.Systems.Superfluid.Agreements.ProportionalDistribution.Common                            as PDCOMMON
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistribution.ConstantFlowDistributionAgreement as CFDA
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistribution.InstantDistributionAgreement      as IDA
import qualified Money.Systems.Superfluid.Agreements.Universal.ConstantFlowAgreement                            as CFA
import qualified Money.Systems.Superfluid.Agreements.Universal.DecayingFlowAgreement                            as DFA
import qualified Money.Systems.Superfluid.Agreements.Universal.InstantTransferAgreement                         as ITA
import qualified Money.Systems.Superfluid.Agreements.Universal.MinterAgreement                                  as MINTA
--
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex                              as PDIDX
import qualified Money.Systems.Superfluid.Agreements.UniversalIndex                                             as UIDX
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency                                        as BBS


-- =====================================================================================================================
-- * Core Types

-- ** Value Type:
--
--   - 18 decimal digit fixed-precision integer.
--   - Name is a monicker of MakerDAO v1 WAD type.

newtype Wad = Wad Integer
    deriving newtype (Default, Eq, Enum, Real, Ord, Num, Integral, Binary, MonetaryValue)

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

-- ** Typed Values:

instance Show (AnyTypedValue Wad) where
    show (AnyTypedValue (p, val)) = show (coerce val :: Wad) ++ "@" ++ typedValueTag p
instance Show (UntappedValue Wad) where show = show . mkAnyTypedValue
instance Show (MVMUD.MintedValue Wad) where show = show . mkAnyTypedValue
instance Show (BBS.BufferValue Wad) where show = show . mkAnyTypedValue

-- ** Timestamp type

-- | Simple timestamp Type.
newtype SimpleTimestamp = SimpleTimestamp Int
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Binary, Timestamp)

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t ++ "s"

-- ** RealTimeBalance Type

-- | Simple Real Time Balance Type.
data SimpleRealTimeBalanceF a = SimpleRealTimeBalanceF
    { untappedValue :: a
    , mintedValue   :: a
    , depositValue  :: a
    }
    deriving stock (Generic, Functor, Foldable, Traversable, Eq)

type SimpleRealTimeBalance = SimpleRealTimeBalanceF Wad

-- *** Lenses of RTB

untappedValueL :: Lens' SimpleRealTimeBalance Wad
untappedValueL  = $(field 'untappedValue)

mintedValueL   :: Lens' SimpleRealTimeBalance Wad
mintedValueL    = $(field 'mintedValue)

depositValueL  :: Lens' SimpleRealTimeBalance Wad
depositValueL   = $(field 'depositValue)

-- *** Class instances of RTB
--

instance Applicative SimpleRealTimeBalanceF where
    pure a = SimpleRealTimeBalanceF a a a
    liftA2 f (SimpleRealTimeBalanceF a b c) (SimpleRealTimeBalanceF a' b' c') =
        SimpleRealTimeBalanceF (f a a') (f b b') (f c c')

instance Show (SimpleRealTimeBalanceF Wad) where
    show rtb =
        (show       . netValueOfRTB      $ rtb) ++ " " ++
        (showDetail . typedValuesFromRTB $ rtb)
        where
        showDetail :: [AnyTypedValue Wad] -> String
        showDetail tvec = "( "
            -- skip zero/default values
            ++ intercalate ", " (map show . filter ((/= def) . exAnyTypedValue) $ tvec)
            ++ " )"

instance Semigroup (SimpleRealTimeBalanceF Wad) where
    (<>) = liftA2 (+)

instance Monoid (SimpleRealTimeBalanceF Wad) where
    mempty = pure 0

instance RealTimeBalance SimpleRealTimeBalanceF Wad where
    valueToRTB _ uval = SimpleRealTimeBalanceF uval def def

    typedValuesToRTB = foldMap g
        where g (AnyTypedValue (p, v)) =
                  let v' = coerce v
                  in case typeRep p of
                      t | t == typeRep (Proxy @(UntappedValue Wad))     -> SimpleRealTimeBalanceF  v' def def
                        | t == typeRep (Proxy @(MVMUD.MintedValue Wad)) -> SimpleRealTimeBalanceF def  v' def
                        | t == typeRep (Proxy @(BBS.BufferValue Wad))   -> SimpleRealTimeBalanceF def def  v'
                        | otherwise -> error ("Invalid value tag: " <> show t)

    typedValuesFromRTB rtb = [ mkAnyTypedValue $ MkUntappedValue     $ untappedValue rtb
                             , mkAnyTypedValue $ MVMUD.MkMintedValue $ mintedValue rtb
                             , mkAnyTypedValue $ BBS.MkBufferValue   $ depositValue rtb
                             ]

-- ** Superfluid Core Types

data SimpleSuperfluidSystem

instance SFTFloat Double

instance SuperfluidCoreTypes SimpleSuperfluidSystem where
    type SFT_FLOAT SimpleSuperfluidSystem = Double
    type SFT_MVAL  SimpleSuperfluidSystem = Wad
    type SFT_TS    SimpleSuperfluidSystem = SimpleTimestamp
    type SFT_RTB_F SimpleSuperfluidSystem = SimpleRealTimeBalanceF

-- =====================================================================================================================
-- * Agreements
--

-- | SimpleMonetaryUnitData type class to capture constraints.
class ( MonetaryUnitDataClass mud SimpleSuperfluidSystem
      , TaggedTypeable mud
      , Show mud
      ) => SimpleMonetaryUnitData mud

-- ** MINTA
--

type SimpleMinterMonetaryUnitData = MINTA.MonetaryUnitData SimpleSuperfluidSystem

instance SimpleMonetaryUnitData SimpleMinterMonetaryUnitData

instance TaggedTypeable SimpleMinterMonetaryUnitData where
    tagFromProxy _ = "Minter"

instance Show SimpleMinterMonetaryUnitData where
    show (MVMUD.MkMonetaryUnitData x) = printf "{ uval = %s, mval = %s }"
        (show $ x^.MVMUD.untappedValue)
        (show $ x^.MVMUD.mintedValue)

-- ** ITA
--

type SimpleITAContractData = ITA.ContractData SimpleSuperfluidSystem

instance TaggedTypeable SimpleITAContractData where
    tagFromProxy _ = "ITA#"

type SimpleITAMonetaryUnitData = ITA.MonetaryUnitData SimpleSuperfluidSystem

instance SimpleMonetaryUnitData SimpleITAMonetaryUnitData

instance TaggedTypeable SimpleITAMonetaryUnitData where
    tagFromProxy _ = "ITA"

instance Show SimpleITAMonetaryUnitData where
    show (IVMUD.MkMonetaryUnitData x) = printf "{ uval = %s }"
        (show $ x^.IVMUD.untappedValue)

-- ** CFA
--

type SimpleCFAMonetaryUnitData = CFA.MonetaryUnitData SimpleSuperfluidSystem

instance TaggedTypeable SimpleCFAMonetaryUnitData where
    tagFromProxy _ = "CFA"

instance Show SimpleCFAMonetaryUnitData where
    show (CFMUD.MkMonetaryUnitData x) = printf "{ t = %s, sval = %s, fr = %s }"
        (show $ x^.CFMUD.settledAt)
        (show $ x^.CFMUD.settledValue)
        (show $ x^.CFMUD.netFlowRate)

instance SimpleMonetaryUnitData SimpleCFAMonetaryUnitData

instance TaggedTypeable SimpleCFAContractData where
    tagFromProxy _ = "CFA#"

instance Show SimpleCFAContractData where
    show ac = printf "{ t_u = %s, fr = %s }"
        (show $ CFA.flow_updated_at ac)
        (show $ CFA.flow_rate ac)

type SimpleCFAContractData = CFA.ContractData SimpleSuperfluidSystem

-- ** DFA
--

type SimpleDFAMonetaryUnitData = DFA.MonetaryUnitData SimpleSuperfluidSystem

instance TaggedTypeable SimpleDFAMonetaryUnitData where
    tagFromProxy _ = "DFA"

instance Show SimpleDFAMonetaryUnitData where
    show (DFMUD.MkMonetaryUnitData x) = printf "{ λ = %s, t = %s, α = %s, ε = %s }"
        (show $ x^.DFMUD.decayingFactor)
        (show $ x^.DFMUD.settledAt)
        (show $ x^.DFMUD.αVal)
        (show $ x^.DFMUD.εVal)

instance TaggedTypeable SimpleDFAContractData where
    tagFromProxy _ = "DFA#"

instance SimpleMonetaryUnitData SimpleDFAMonetaryUnitData

type SimpleDFAContractData = DFA.ContractData SimpleSuperfluidSystem

instance Show SimpleDFAContractData where
    show ac = printf "{ t_u = %s, δ = %s }"
        (show $ DFA.flow_last_updated_at ac)
        (show $ DFA.distribution_limit ac)

-- ** IDA
--

instance TaggedTypeable (IDA.PublisherMonetaryUnitData SimpleSuperfluidSystem) where
    tagFromProxy _ = "IDA(P)"

instance Show (IDA.PublisherMonetaryUnitData SimpleSuperfluidSystem) where
    show (IVMUD.MkMonetaryUnitData x) = printf "pub { uval = %s }"
        (show $ x^.IVMUD.untappedValue)

instance SimpleMonetaryUnitData (IDA.PublisherMonetaryUnitData SimpleSuperfluidSystem)

instance TaggedTypeable (IDA.SubscriberMonetaryUnitData SimpleSuperfluidSystem) where
    tagFromProxy _ = "IDA(S)"

instance Show (IDA.SubscriberMonetaryUnitData SimpleSuperfluidSystem) where
    show (IVMUD.MkMonetaryUnitData x) = printf "sub { uval = %s }"
        (show $ x^.IVMUD.untappedValue)

instance SimpleMonetaryUnitData (IDA.SubscriberMonetaryUnitData SimpleSuperfluidSystem)

-- ** CFDA
--

instance TaggedTypeable (CFDA.PublisherMonetaryUnitData SimpleSuperfluidSystem) where
    tagFromProxy _ = "CFDA(P)"

instance Show (CFDA.PublisherMonetaryUnitData SimpleSuperfluidSystem) where
    show (CFMUD.MkMonetaryUnitData x) = printf "pub { t = %s, sval = %s, fr = %s }"
        (show $ x^.CFMUD.settledAt)
        (show $ x^.CFMUD.settledValue)
        (show $ x^.CFMUD.netFlowRate)

instance SimpleMonetaryUnitData (CFDA.PublisherMonetaryUnitData SimpleSuperfluidSystem)

instance TaggedTypeable (CFDA.SubscriberMonetaryUnitData SimpleSuperfluidSystem) where
    tagFromProxy _ = "CFDA(S)"

instance Show (CFDA.SubscriberMonetaryUnitData SimpleSuperfluidSystem) where
    show (CFMUD.MkMonetaryUnitData x) = printf "sub { t = %s, sval = %s, fr = %s }"
        (show $ x^.CFMUD.settledAt)
        (show $ x^.CFMUD.settledValue)
        (show $ x^.CFMUD.netFlowRate)

instance SimpleMonetaryUnitData (CFDA.SubscriberMonetaryUnitData SimpleSuperfluidSystem)

-- =====================================================================================================================
-- * Indexes

-- ** UIDX

type SimpleUniversalData = UIDX.UniversalData SimpleSuperfluidSystem

-- ** PDIDX

type ProportionalDistributionIndexID = Int
type SimpleDistributionContract = PDIDX.DistributionContract SimpleSuperfluidSystem
type SimpleSubscriptionContract = PDIDX.SubscriptionContract SimpleSuperfluidSystem

instance TaggedTypeable (PDIDX.SubscriptionContract SimpleSuperfluidSystem) where
    tagFromProxy _ = "PD(S)#"
instance TaggedTypeable (PDIDX.DistributionContract SimpleSuperfluidSystem) where
    tagFromProxy _ = "PD(P)#"

deriving instance Show (PDCOMMON.DistributionContractBase SimpleSuperfluidSystem)
deriving instance Show (PDCOMMON.SubscriptionContractBase SimpleSuperfluidSystem)
deriving instance Show (IDA.DistributionContract SimpleSuperfluidSystem)
deriving instance Show (IDA.SubscriptionContract SimpleSuperfluidSystem)
deriving instance Show (CFDA.DistributionContract SimpleSuperfluidSystem)
deriving instance Show (CFDA.SubscriptionContract SimpleSuperfluidSystem)
deriving instance Show (PDIDX.DistributionContract SimpleSuperfluidSystem)
deriving instance Show (PDIDX.SubscriptionContract SimpleSuperfluidSystem)

type SimplePublisherData = PDIDX.PublisherData SimpleSuperfluidSystem
type SimpleSubscriberData = PDIDX.SubscriberData SimpleSuperfluidSystem

-- =====================================================================================================================
-- * System Types

-- ** AnySimpleMonetaryUnitData
--

-- | AnyMonetaryUnitData type.
data AnySimpleMonetaryUnitData = forall mud. SimpleMonetaryUnitData mud => MkAnySimpleMonetaryUnitData mud

instance MonetaryUnitDataClass AnySimpleMonetaryUnitData SimpleSuperfluidSystem where
    balanceProvided (MkAnySimpleMonetaryUnitData a) = balanceProvided a

instance TaggedTypeable AnySimpleMonetaryUnitData where
    tagFromProxy _ = "Any#"

instance Show AnySimpleMonetaryUnitData where
    show (MkAnySimpleMonetaryUnitData a) = show a

instance SimpleMonetaryUnitData AnySimpleMonetaryUnitData

-- Conjouring some type tricks to define the any type.
deriving instance SimpleMonetaryUnitData mud
    => MPTC_Flip MonetaryUnitDataClass SimpleSuperfluidSystem mud
instance AnySimpleMonetaryUnitData `IsAnyTypeOf` SimpleMonetaryUnitData where
    mkAny _ = MkAnySimpleMonetaryUnitData

-- deriving instance MPTC_Flip MonetaryUnitDataClass SimpleSuperfluidSystem AnySimpleMonetaryUnitData

-- ** Some useful AgreementOperationOutputF instances
--

instance ( Foldable (AgreementOperationOutputF ao)
         , Eq elem
         ) => Eq (AgreementOperationOutputF ao elem) where
    a == b = and $ zipWith (==) (toList a) (toList b)

instance ( Foldable (AgreementOperationOutputF ao)
         , Ord elem
         ) => Ord (AgreementOperationOutputF ao elem) where
    a <= b = and $ zipWith (<=) (toList a) (toList b)

instance ( Foldable (AgreementOperationOutputF ao)
         , Show elem
         ) => Show (AgreementOperationOutputF ao elem) where
    show elems = "(" ++ (elems & toList & map show & intercalate ", ") ++ ")"

-- ** Superfluid System Types

instance SuperfluidSystemTypes SimpleSuperfluidSystem where
    type SFT_ANY_MUD SimpleSuperfluidSystem = AnySimpleMonetaryUnitData
    dfa_default_lambda _ = log 2 / (3600 * 24 * 7)
