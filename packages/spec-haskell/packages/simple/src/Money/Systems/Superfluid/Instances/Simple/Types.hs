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
    -- SimpleTimestamp
    , SimpleTimestamp (..)
    -- SimpleRealtimeBalance
    , SimpleRealtimeBalance (..)
    -- SimpleSuperfluidTypes
    , SimpleSuperfluidTypes
    -- Agreement
    , AnySimpleAgreementContractData (..)
    , AnySimpleAgreementMonetaryUnitData (..)
    ) where

import           Control.Exception                                            (assert)
import           Data.Binary
import           Data.Default
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
    type SFT_LQ SimpleSuperfluidTypes = Wad
    type SFT_TS SimpleSuperfluidTypes = SimpleTimestamp
    type SFT_RTB SimpleSuperfluidTypes = SimpleRealtimeBalance

-- ============================================================================
-- Agreement Types
--

-- ITA

instance TaggedTypeable (UIDX.ITAMonetaryUnitData SimpleSuperfluidTypes) where
    tagFromProxy _ = "ITA@U"

instance TaggedTypeable (UIDX.ITAContractData SimpleSuperfluidTypes) where
    tagFromProxy _ = "ITA#"

instance Show (UIDX.ITAMonetaryUnitData SimpleSuperfluidTypes) where
    show (ITA.MkMonetaryUnitData x) = printf "{ uval = %s, mval = %s }"
        (show $ x^.ITA.untappedValue)
        (show $ x^.ITA.mintedValue)

-- CFA

instance TaggedTypeable (UIDX.CFAMonetaryUnitData SimpleSuperfluidTypes) where
    tagFromProxy _ = "CFA@U"

instance TaggedTypeable (UIDX.CFAContractData SimpleSuperfluidTypes) where
    tagFromProxy _ = "CFA#"

instance Show (UIDX.CFAContractData SimpleSuperfluidTypes) where
    show (CFA.MkContractData x) = printf "{ flowLastUpdatedAt = %s, flowRate = %s, flowBuffer = %s }"
        (show $ x^.CFA.flowLastUpdatedAt)
        (show $ x^.CFA.flowRate)
        (show $ x^.CFA.flowBuffer)

instance Show (UIDX.CFAMonetaryUnitData SimpleSuperfluidTypes) where
    show (CFA.MkMonetaryUnitData x) = printf "{ t = %s, uval = %s, buf = %s, fr = %s }"
        (show $ x^.CFA.settledAt)
        (show $ x^.CFA.settledUntappedValue)
        (show $ x^.CFA.settledBufferValue)
        (show $ x^.CFA.netFlowRate)

-- DFA

instance TaggedTypeable (UIDX.DFAMonetaryUnitData SimpleSuperfluidTypes) where
    tagFromProxy _ = "DFA@U"

instance TaggedTypeable (UIDX.DFAContractData SimpleSuperfluidTypes) where
    tagFromProxy _ = "DFA#"

instance Show (UIDX.DFAContractData SimpleSuperfluidTypes) where
    show (DFA.MkContractData x) = printf "{ t_u = %s, δ = %s }"
        (show $ x^.DFA.flowLastUpdatedAt)
        (show $ x^.DFA.distributionLimit)

instance Show (UIDX.DFAMonetaryUnitData SimpleSuperfluidTypes) where
    show (DFA.MkMonetaryUnitData x) = printf "{ λ = %s, t_s = %s, α = %s, ε = %s, buf = %s }"
        (show $ x^.DFA.decayingFactor)
        (show $ x^.DFA.settledAt)
        (show $ x^.DFA.αVal)
        (show $ x^.DFA.εVal)
        (show $ x^.DFA.settledBuffer)

-- | AnyAgreementContractData type.
data AnySimpleAgreementContractData = forall acd amu.
    ( AgreementContractData acd amu SimpleSuperfluidTypes
    , TaggedTypeable acd
    , Show acd
    ) => MkSimpleAgreementContractData acd

instance Show AnySimpleAgreementContractData where
   show (MkSimpleAgreementContractData g) = show g

-- | AnyAgreementMonetaryUnitData type.
data AnySimpleAgreementMonetaryUnitData = forall amu.
    ( AgreementMonetaryUnitData amu SimpleSuperfluidTypes
    , TaggedTypeable amu
    , Show amu
    ) => MkSimpleAgreementMonetaryUnitData amu

instance Show AnySimpleAgreementMonetaryUnitData where
    show (MkSimpleAgreementMonetaryUnitData a) = show a
