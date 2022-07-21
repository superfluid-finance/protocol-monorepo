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
    , SimpleMINTAContractData
    , SimpleMINTAMonetaryUnitData
    , SimpleITAContractData
    , SimpleITAMonetaryUnitData
    , SimpleCFAContractData
    , SimpleCFAMonetaryUnitData
    , SimpleDFAContractData
    , SimpleDFAMonetaryUnitData
    , AnySimpleAgreementContractData (..)
    , AnySimpleAgreementMonetaryUnitData (..)
    ) where

import           Control.Applicative                                            (Applicative (..))
import           Data.Binary
import           Data.Default
import           Data.Foldable                                                  (toList)
import           Data.List                                                      (intercalate)
import           Data.Proxy
import           Data.Type.TaggedTypeable
import           GHC.Generics                                                   (Generic)
import           Lens.Internal
import           Text.Printf                                                    (printf)


import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement      as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement      as DFA
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement   as ITA
import qualified Money.Systems.Superfluid.Agreements.MinterAgreement            as MINTA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency        as BBS
--
import qualified Money.Systems.Superfluid.Indexes.ProportionalDistributionIndex as PDIDX
import qualified Money.Systems.Superfluid.Indexes.UniversalIndex                as UIDX


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
-- Type Values:

instance Show (UntappedValue Wad) where
    show (UntappedValue val) = show val ++ "@_"

instance TypedValueTag vtag => Show (TappedValue vtag Wad) where
    show (TappedValue val) = show val ++ "@" ++ tappedValueTag (Proxy @vtag)

instance Show (AnyTappedValue Wad) where
    show (AnyTappedValue (MkTappedValueTag vtagProxy, val)) = show val ++ "@" ++ tappedValueTag vtagProxy

-- =====================================================================================================================
-- Timestamp type

-- | Simple timestamp Type.
newtype SimpleTimestamp = SimpleTimestamp Int
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Binary, Timestamp)

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t ++ "s"

-- =====================================================================================================================
-- RealTimeBalance Type

-- | Simple Real Time Balance Type.
data SimpleRealTimeBalanceF a = SimpleRealTimeBalanceF
    { untappedValue :: a
    , mintedValue   :: a
    , depositValue  :: a
    }
    deriving stock (Generic, Functor, Foldable, Traversable)
    deriving anyclass (Binary, Default)

type SimpleRealTimeBalance = SimpleRealTimeBalanceF Wad

untappedValueL :: Lens' SimpleRealTimeBalance Wad
untappedValueL  = lensOfRTB untappedValueTag
mintedValueL   :: Lens' SimpleRealTimeBalance Wad
mintedValueL    = lensOfRTB MINTA.mintedValueTag
depositValueL   :: Lens' SimpleRealTimeBalance Wad
depositValueL   = lensOfRTB BBS.bufferValueTag

--  deriving (Num, Show) via RTBDerivingHelper (SimpleRealTimeBalanceF Wad) Wad

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
            ++ foldl ((++) . (++ ", ")) "" ((map show) . (filter ((/= def) . untypeValue )) $ tvec)
            ++ " )"

instance Semigroup (SimpleRealTimeBalanceF Wad) where
    (<>) = liftA2 (+)

instance Monoid (SimpleRealTimeBalanceF Wad) where
    mempty   = pure 0

instance RealTimeBalance SimpleRealTimeBalanceF Wad where
    valueToRTB uval = SimpleRealTimeBalanceF uval def def

    typedValuesToRTB (UntappedValue uval) tvec =
        (SimpleRealTimeBalanceF uval def def) <> (flip foldMap tvec g)
        -- extra correctly typed RTB monoid
        where g = \(AnyTappedValue (MkTappedValueTag p, v)) -> case typeRep p of
                  t | t == typeRep MINTA.mintedValueTag -> SimpleRealTimeBalanceF def   v def
                    | t == typeRep BBS.bufferValueTag   -> SimpleRealTimeBalanceF def def   v
                    | otherwise -> error "Invalid monetary value tag"

    typedValuesFromRTB rtb = (UntappedValue (untappedValue rtb),
                              [ mkAnyTappedValue $ MINTA.mkMintedValue $ mintedValue rtb
                              , mkAnyTappedValue $ BBS.mkBufferValue   $ depositValue rtb
                              ])

    lensOfRTB p | t == typeRep untappedValueTag      = $(field 'untappedValue)
                | t == typeRep MINTA.mintedValueTag  = $(field 'mintedValue)
                | t == typeRep BBS.bufferValueTag    = $(field 'depositValue)
                | otherwise = error "Invalid monetary value tag"
        where t = typeRep p

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
-- Agreement Types
--

-- * MINTA types.
--

type SimpleMINTAContractData = UIDX.MINTAContractData SimpleSuperfluidTypes

instance TaggedTypeable SimpleMINTAContractData where
    tagFromProxy _ = "MINTA#"

type SimpleMINTAMonetaryUnitData = UIDX.MINTAMonetaryUnitData SimpleSuperfluidTypes

instance TaggedTypeable SimpleMINTAMonetaryUnitData where
    tagFromProxy _ = "MINTA"

instance Show SimpleMINTAMonetaryUnitData where
    show (MINTA.MkMonetaryUnitData x) = printf "{ uval = %s, mval = %s }"
        (show $ x^.MINTA.untappedValue)
        (show $ x^.MINTA.mintedValue)

-- * ITA types.
--

type SimpleITAContractData = UIDX.ITAContractData SimpleSuperfluidTypes

instance TaggedTypeable SimpleITAContractData where
    tagFromProxy _ = "ITA#"

type SimpleITAMonetaryUnitData = UIDX.ITAMonetaryUnitData SimpleSuperfluidTypes

instance TaggedTypeable SimpleITAMonetaryUnitData where
    tagFromProxy _ = "ITA"

instance Show SimpleITAMonetaryUnitData where
    show (ITA.MkMonetaryUnitData x) = printf "{ uval = %s }"
        (show $ x^.ITA.untappedValue)

-- * IDA types.

instance TaggedTypeable (PDIDX.IDAPublisherContractData SimpleSuperfluidTypes) where
    tagFromProxy _ = "IDA(P)#"

instance TaggedTypeable (PDIDX.IDAPublisherMonetaryUnitData SimpleSuperfluidTypes) where
    tagFromProxy _ = "IDA(P)"

instance Show (PDIDX.IDAPublisherMonetaryUnitData SimpleSuperfluidTypes) where
    show (ITA.MkMonetaryUnitData x) = printf "{ uval = %s }"
        (show $ x^.ITA.untappedValue)

instance TaggedTypeable (PDIDX.IDASubscriberContractData SimpleSuperfluidTypes) where
    tagFromProxy _ = "IDA(S)#"

instance TaggedTypeable (PDIDX.IDASubscriberMonetaryUnitData SimpleSuperfluidTypes) where
    tagFromProxy _ = "IDA(S)"

instance Show (PDIDX.IDASubscriberMonetaryUnitData SimpleSuperfluidTypes) where
    show (ITA.MkMonetaryUnitData x) = printf "{ uval = %s }"
        (show $ x^.ITA.untappedValue)

-- * CFA types.
--

type SimpleCFAContractData = UIDX.CFAContractData SimpleSuperfluidTypes

instance TaggedTypeable SimpleCFAContractData where
    tagFromProxy _ = "CFA#"

instance Show SimpleCFAContractData where
    show (CFA.MkContractData x) = printf "{ flowLastUpdatedAt = %s, flowRate = %s, flowBuffer = %s }"
        (show $ x^.CFA.flowLastUpdatedAt)
        (show $ x^.CFA.flowRate)
        (show $ x^.CFA.flowBuffer)

type SimpleCFAMonetaryUnitData = UIDX.CFAMonetaryUnitData SimpleSuperfluidTypes

instance Show SimpleCFAMonetaryUnitData where
    show (CFA.MkMonetaryUnitData x) = printf "{ t = %s, uval = %s, buf = %s, fr = %s }"
        (show $ x^.CFA.settledAt)
        (show $ x^.CFA.settledUntappedValue)
        (show $ x^.CFA.settledBufferValue)
        (show $ x^.CFA.netFlowRate)

instance TaggedTypeable SimpleCFAMonetaryUnitData where
    tagFromProxy _ = "CFA"

-- * DFA types.
--

type SimpleDFAContractData = UIDX.DFAContractData SimpleSuperfluidTypes

instance Show SimpleDFAContractData where
    show (DFA.MkContractData x) = printf "{ t_u = %s, δ = %s }"
        (show $ x^.DFA.flowLastUpdatedAt)
        (show $ x^.DFA.distributionLimit)

instance TaggedTypeable SimpleDFAContractData where
    tagFromProxy _ = "DFA#"

type SimpleDFAMonetaryUnitData = UIDX.DFAMonetaryUnitData SimpleSuperfluidTypes

instance TaggedTypeable SimpleDFAMonetaryUnitData where
    tagFromProxy _ = "DFA"

instance Show SimpleDFAMonetaryUnitData where
    show (DFA.MkMonetaryUnitData x) = printf "{ λ = %s, t_s = %s, α = %s, ε = %s, buf = %s }"
        (show $ x^.DFA.decayingFactor)
        (show $ x^.DFA.settledAt)
        (show $ x^.DFA.αVal)
        (show $ x^.DFA.εVal)
        (show $ x^.DFA.settledBuffer)

-- * AnyX types.
--

-- | AnyAgreementContractData type.
data AnySimpleAgreementContractData = forall acd amud.
    ( AgreementContractData acd amud SimpleSuperfluidTypes
    , TaggedTypeable acd
    , Show acd
    ) => MkSimpleAgreementContractData acd

instance Show AnySimpleAgreementContractData where
   show (MkSimpleAgreementContractData g) = show g

-- | AnyAgreementMonetaryUnitData type.
data AnySimpleAgreementMonetaryUnitData = forall amud.
    ( AgreementMonetaryUnitData amud SimpleSuperfluidTypes
    , TaggedTypeable amud
    , Show amud
    ) => MkSimpleAgreementMonetaryUnitData amud

instance Show AnySimpleAgreementMonetaryUnitData where
    show (MkSimpleAgreementMonetaryUnitData a) = show a

-- * Some useful AgreementContractPartiesF instances
--

instance ( Foldable (AgreementContractPartiesF acd)
         , Eq elem
         ) => Eq (AgreementContractPartiesF acd elem) where
    a == b = foldr (&&) True $ zipWith (==) (toList a) (toList b)

instance ( Foldable (AgreementContractPartiesF acd)
         , Ord elem
         ) => Ord (AgreementContractPartiesF acd elem) where
    a <= b = foldr (&&) True $ zipWith (<=) (toList a) (toList b)

instance ( Foldable (AgreementContractPartiesF acd)
         , Show elem
         ) => Show (AgreementContractPartiesF acd elem) where
    show elems = "(" ++ (elems & toList & map show & intercalate ", ") ++ ")"
