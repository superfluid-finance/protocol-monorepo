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
    -- , AnySimpleAgreementContractData (..)
    , AnySimpleAgreementAccountData (..)
    ) where

import           Control.Exception                                                (assert)
import           Data.Binary
import           Data.Default
import           Data.Proxy
import           Data.Type.TaggedTypeable
import           GHC.Generics                                                     (Generic)
import           Text.Printf                                                      (printf)

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency          as BBS


-- ============================================================================
-- SFDouble Type
--
newtype SFDouble = SFDouble Double
    deriving newtype (Default, Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Show)
instance SFTFloat SFDouble

-- ============================================================================
-- Wad Type:
--   * 18 decimal digit fixed-precision integer
--   * an instance of Value
--
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

instance Show (UntappedValue Wad) where
    show (UntappedValue val) = show val ++ "@_"

instance (TappedValueTag vtag) => Show (TappedValue vtag Wad) where
    show (TappedValue val) = show val ++ "@" ++ tagFromProxy (Proxy @vtag)

instance Show (AnyTappedValue Wad) where
    show (AnyTappedValue (MkTappedLiquidityTag tag, val)) = show val ++ "@" ++ tagFromProxy tag


-- | Simple timestamp Type .
newtype SimpleTimestamp = SimpleTimestamp Int
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Binary, Timestamp)

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t ++ "s"

-- | Simple realtime balance Type.
data SimpleRealtimeBalance = SimpleRealtimeBalance
    { untappedLiquidityVal :: Wad
    , mintedVal            :: Wad
    , depositVal           :: Wad
    , owedDepositVal       :: Wad
    }
    deriving stock (Generic)
    deriving anyclass (Binary, Default)
    deriving (Num, Show) via RealtimeBalanceDerivingHelper SimpleRealtimeBalance Wad

instance Show (RealtimeBalanceDerivingHelper SimpleRealtimeBalance Wad) where
    show (RealtimeBalanceDerivingHelper rtb) =
        (show . valueRequiredForRTB $ rtb) ++ " " ++
        (showDetail . typedLiquidityVectorFromRTB $ rtb)
        where
        showDetail (TypedLiquidityVector uval tvec) = "( "
            ++ show uval
            -- This is a version that ignores any zero value scalar:
            -- ++ foldl ((++) . (++ ", ")) "" ((map show) . (filter ((/= def) . untypeLiquidity )) $ tvec)
            ++ foldl ((++) . (++ ", ")) "" (map show tvec)
            ++ " )"

instance RealtimeBalance SimpleRealtimeBalance Wad where
    typedLiquidityVectorFromRTB rtb = TypedLiquidityVector
        ( UntappedValue $ untappedLiquidityVal rtb)
        [ mkAnyTappedLiquidity $ TBA.mkMintedLiquidity $ mintedVal rtb
        , mkAnyTappedLiquidity $ BBS.mkBufferLiquidity $ depositVal rtb
        ]

    valueToRTB uval = SimpleRealtimeBalance uval def def def

    untypedLiquidityVectorToRTB (UntypedLiquidityVector uval uvec) = assert (length uvec == 3) $
        SimpleRealtimeBalance uval (head uvec) (uvec!!1) (uvec!!2)

    typedLiquidityVectorToRTB (TypedLiquidityVector (UntappedValue uval) tvec) =
        SimpleRealtimeBalance uval mval d od
        where d = foldr ((+) . (`fromAnyTappedLiquidity` BBS.bufferLiquidityTag)) def tvec
              mval = foldr ((+) . (`fromAnyTappedLiquidity` TBA.mintedLiquidityTag)) def tvec
              od = def

data SimpleSuperfluidTypes

instance SuperfluidTypes SimpleSuperfluidTypes where
    type SFT_FLOAT SimpleSuperfluidTypes = SFDouble
    type SFT_LQ SimpleSuperfluidTypes = Wad
    type SFT_TS SimpleSuperfluidTypes = SimpleTimestamp
    type SFT_RTB SimpleSuperfluidTypes = SimpleRealtimeBalance

-- ============================================================================
-- Agreement Types
--
instance Show (TBA.AccountData SimpleSuperfluidTypes) where
    show x = printf "{ uval = %s, mval = %s }"
        (show $ TBA.untappedLiquidity x)
        (show $ TBA.mintedLiquidity x)

instance Show (CFA.ContractData SimpleSuperfluidTypes) where
    show x = printf "{ flowLastUpdatedAt = %s, flowRate = %s, flowBuffer = %s }"
        (show $ CFA.flowLastUpdatedAt x) (show $ CFA.flowRate x) (show $ CFA.flowBuffer x)

instance Show (CFA.AccountData SimpleSuperfluidTypes) where
    show x = printf "{ t = %s, uval = %s, buf = %s, fr = %s }"
        (show $ CFA.settledAt x)
        (show $ CFA.settledUntappedLiquidity x)
        (show $ CFA.settledBufferLiquidity x)
        (show $ CFA.netFlowRate x)

instance Show (DFA.ContractData SimpleSuperfluidTypes) where
    show x = printf "{ t_u = %s, δ = %s, λ = %s }"
        (show $ DFA.flowLastUpdatedAt x) (show $ DFA.distributionLimit x) (show $ DFA.decayingFactor x)

instance Show (DFA.AccountData SimpleSuperfluidTypes) where
    show x = printf "{ t_s = %s, α = %s, ε = %s, buf = %s }"
        (show $ DFA.settledAt x)
        (show $ DFA.αVal x)
        (show $ DFA.εVal x)
        (show $ DFA.settledBuffer x)

-- | AnyAgreementContractData type
-- data AnySimpleAgreementContractData =
--     forall a. ( Agreement a
--               , DistributionForAgreement a ~ SimpleSuperfluidTypes
--               , Serializable (AgreementContractData a) SimpleSuperfluidTypes
--               )
--     => MkSimpleAgreementContractData (AgreementContractData a)

-- instance Show AnySimpleAgreementContractData where
--    show (MkSimpleAgreementContractData g) = show g

-- instance Serializable AnySimpleAgreementContractData SimpleSuperfluidTypes where
--     getter = undefined -- not possible, and no need to define
--     putter (MkSimpleAgreementContractData a) = putter a

-- | AnyAgreementAccountData type
data AnySimpleAgreementAccountData =
    forall a. ( Agreement a SimpleSuperfluidTypes
              , Show (AgreementAccountData a)
              )
    => MkSimpleAgreementAccountData (AgreementAccountData a)

instance Show AnySimpleAgreementAccountData where
    show (MkSimpleAgreementAccountData a) = show a
