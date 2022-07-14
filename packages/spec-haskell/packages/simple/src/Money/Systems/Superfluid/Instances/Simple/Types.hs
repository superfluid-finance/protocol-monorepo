{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeFamilies   #-}

module Money.Systems.Superfluid.Instances.Simple.Types
    ( module Money.Systems.Superfluid.Concepts
    -- Double
    , SFDouble (..)
    -- Wad
    , Wad (..)
    , toWad
    , wad4humanN
    , wad4human
    -- Timestamp
    , SimpleTimestamp (..)
    -- RealtimeBalance
    , SimpleRealtimeBalance (..)
    -- SuperfluidTypes
    , SimpleSuperfluidTypes
    -- Agreements
    , SimpleITAContractData
    , SimpleITAMonetaryUnitData
    , SimpleCFAContractData
    , SimpleCFAMonetaryUnitData
    , SimpleDFAContractData
    , SimpleDFAMonetaryUnitData
    , AnySimpleAgreementContractData (..)
    , AnySimpleAgreementMonetaryUnitData (..)
    ) where

import           Control.Exception                                            (assert)
import           Data.Binary
import           Data.Default
import           Data.Foldable                                                (toList)
import           Data.List                                                    (intercalate)
import           Data.Proxy
import           Data.Type.TaggedTypeable
import           GHC.Generics                                                 (Generic)
import           Lens.Micro
import           Text.Printf                                                  (printf)


import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement    as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement    as DFA
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement as ITA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency      as BBS
--
import qualified Money.Systems.Superfluid.Indexes.UniversalIndexes            as UIDX


-- ============================================================================
-- SFDouble Type

newtype SFDouble = SFDouble Double
    deriving newtype (Default, Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Show)
instance SFTFloat SFDouble

-- ============================================================================
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

-- ============================================================================
-- Type Values:

instance Show (UntappedValue Wad) where
    show (UntappedValue val) = show val ++ "@_"

instance TappedValueTag vtag => Show (TappedValue vtag Wad) where
    show (TappedValue val) = show val ++ "@" ++ tappedValueTag (Proxy @vtag)

instance Show (AnyTappedValue Wad) where
    show (AnyTappedValue (MkTappedValueTag vtagProxy, val)) = show val ++ "@" ++ tappedValueTag vtagProxy

-- ============================================================================
-- Timestamp type

-- | Simple timestamp Type.
newtype SimpleTimestamp = SimpleTimestamp Int
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Binary, Timestamp)

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t ++ "s"

-- ============================================================================
-- RealtimeBalance Type

-- | Simple realtime balance Type.
data SimpleRealtimeBalance = SimpleRealtimeBalance
    { untappedValueVal :: Wad
    , mintedVal        :: Wad
    , depositVal       :: Wad
    , owedDepositVal   :: Wad
    }
    deriving stock (Generic)
    deriving anyclass (Binary, Default)
    deriving (Num, Show) via RealtimeBalanceDerivingHelper SimpleRealtimeBalance Wad

instance Show (RealtimeBalanceDerivingHelper SimpleRealtimeBalance Wad) where
    show (RealtimeBalanceDerivingHelper rtb) =
        (show       . valueRequiredForRTB     $ rtb) ++ " " ++
        (showDetail . typedValueVectorFromRTB $ rtb)
        where
        showDetail (TypedValueVector uval tvec) = "( "
            ++ show uval
            ++ foldl ((++) . (++ ", ")) "" ((map show) . (filter ((/= def) . getUntypedValue )) $ tvec)
            ++ " )"

instance RealtimeBalance SimpleRealtimeBalance Wad where
    typedValueVectorFromRTB rtb = TypedValueVector
        ( UntappedValue $ untappedValueVal rtb)
        [ mkAnyTappedValue $ ITA.mkMintedValue $ mintedVal rtb
        , mkAnyTappedValue $ BBS.mkBufferValue $ depositVal rtb
        ]

    valueToRTB uval = SimpleRealtimeBalance uval def def def

    untypedValueVectorToRTB (UntypedValueVector uval uvec) = assert (length uvec == 3) $
        SimpleRealtimeBalance uval (head uvec) (uvec!!1) (uvec!!2)

    typedValueVectorToRTB (TypedValueVector (UntappedValue uval) tvec) =
        SimpleRealtimeBalance uval mval d od
        where d = foldr ((+) . (`fromAnyTappedValue` BBS.bufferValueTag)) def tvec
              mval = foldr ((+) . (`fromAnyTappedValue` ITA.mintedValueTag)) def tvec
              od = def

-- ============================================================================
-- SuperfluidTypes Type

data SimpleSuperfluidTypes

instance SuperfluidTypes SimpleSuperfluidTypes where
    type SFT_FLOAT SimpleSuperfluidTypes = SFDouble
    type SFT_MVAL SimpleSuperfluidTypes = Wad
    type SFT_TS SimpleSuperfluidTypes = SimpleTimestamp
    type SFT_RTB SimpleSuperfluidTypes = SimpleRealtimeBalance

-- ============================================================================
-- Agreement Types
--

-- ITA

type SimpleITAContractData = UIDX.ITAContractData SimpleSuperfluidTypes

instance TaggedTypeable SimpleITAContractData where
    tagFromProxy _ = "ITA#"

type SimpleITAMonetaryUnitData = UIDX.ITAMonetaryUnitData SimpleSuperfluidTypes

instance TaggedTypeable SimpleITAMonetaryUnitData where
    tagFromProxy _ = "ITA"

instance Show SimpleITAMonetaryUnitData where
    show (ITA.MkMonetaryUnitData x) = printf "{ uval = %s, mval = %s }"
        (show $ x^.ITA.untappedValue)
        (show $ x^.ITA.mintedValue)

-- CFA

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

-- DFA

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

-- Any

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
