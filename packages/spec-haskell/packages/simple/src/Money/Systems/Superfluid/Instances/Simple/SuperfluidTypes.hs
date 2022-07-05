{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Money.Systems.Superfluid.Instances.Simple.SuperfluidTypes
    ( module Money.Systems.Superfluid.Concepts.Liquidity
    -- Serializable
    , Putter (..)
    , Getter (..)
    , Serializable (..)
    , Serialized (..)
    -- Double
    , SFDouble (..)
    -- SimpleAddress
    , SimpleAddress
    , createSimpleAddress
    -- Wad
    , Wad (..)
    , toWad
    , wad4humanN
    , wad4human
    -- SimpleTimestamp
    , SimpleTimestamp (..)
    -- SimpleRealtimeBalance
    , module Money.Systems.Superfluid.Concepts.RealtimeBalance
    , SimpleRealtimeBalance (..)
    -- SimpleSuperfluidTypes
    , SimpleSuperfluidTypes
    -- Agreement
    -- , AnySimpleAgreementContractData (..)
    , AnySimpleAgreementAccountData (..)
    ) where

import           Control.Exception                                                (assert)
import           Data.Binary
import           Data.Char                                                        (isAlpha)
import           Data.Default
import           Data.Maybe
import           Data.Proxy
import           Data.String
import           Data.Type.TaggedTypeable
import           GHC.Generics                                                     (Generic)
import           Text.Printf                                                      (printf)

import           Money.Systems.Superfluid.Concepts.Agreement
import           Money.Systems.Superfluid.Concepts.Liquidity
import           Money.Systems.Superfluid.Concepts.RealtimeBalance
import           Money.Systems.Superfluid.Concepts.SuperfluidTypes
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency          as BBS


-- ============================================================================
-- Putter/Getter Serializable Framework
--

class (Monad srl, SuperfluidTypes sft) => Putter srl sft | srl -> sft where
    putFloat :: SFT_FLOAT sft -> srl ()
    putLQ :: SFT_LQ sft -> srl ()
    putTS :: SFT_TS sft -> srl ()

class (Monad srl, SuperfluidTypes sft) => Getter srl sft | srl -> sft where
    getFloat :: srl (SFT_FLOAT sft)
    getLQ :: srl (SFT_LQ sft)
    getTS :: srl (SFT_TS sft)

class SuperfluidTypes sft => Serializable a sft | a -> sft where
    getter :: Getter srl sft => Proxy a -> srl a
    putter :: Putter srl sft => a -> srl ()

class SuperfluidTypes sft => Serialized s sft | s -> sft where
    runGetter :: Serializable a sft => Proxy a -> s -> a
    runPutter :: Serializable a sft => a -> s

-- ============================================================================
-- SFDouble Type
--
newtype SFDouble = SFDouble Double
    deriving newtype (Default, Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat, Show)
instance SFTFloat SFDouble

-- ============================================================================
-- Wad Type:
--   * 18 decimal digit fixed-precision integer
--   * an instance of Liquidity
--
newtype Wad = Wad Integer
    deriving newtype (Default, Eq, Enum, Real, Ord, Num, Integral, Binary, Liquidity)

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

instance Show (UntappedLiquidity Wad) where
    show (UntappedLiquidity liq) = show liq ++ "@_"

instance (TappedLiquidityTag ltag) => Show (TappedLiquidity ltag Wad) where
    show (TappedLiquidity liq) = show liq ++ "@" ++ tagFromProxy (Proxy @ltag)

instance Show (AnyTappedLiquidity Wad) where
    show (AnyTappedLiquidity (MkTappedLiquidityTag tag, liq)) = show liq ++ "@" ++ tagFromProxy tag


-- ============================================================================
-- SimpleTimestamp Type
--
newtype SimpleTimestamp = SimpleTimestamp Int
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Binary)
    deriving anyclass (Timestamp)

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t ++ "s"

-- ============================================================================
-- SimpleRealtimeBalance Type
--
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
        (show . liquidityRequiredForRTB $ rtb) ++ " " ++
        (showDetail . typedLiquidityVectorFromRTB $ rtb)
        where
        showDetail (TypedLiquidityVector uliq tvec) = "( "
            ++ show uliq
            -- This is a version that ignores any zero liquidity scalar:
            -- ++ foldl ((++) . (++ ", ")) "" ((map show) . (filter ((/= def) . untypeLiquidity )) $ tvec)
            ++ foldl ((++) . (++ ", ")) "" (map show tvec)
            ++ " )"

instance RealtimeBalance SimpleRealtimeBalance Wad where
    liquidityVectorFromRTB rtb = map (`id` rtb) [untappedLiquidityVal, mintedVal, depositVal, owedDepositVal]

    typedLiquidityVectorFromRTB rtb = TypedLiquidityVector
        ( UntappedLiquidity $ untappedLiquidityVal rtb)
        [ mkAnyTappedLiquidity $ TBA.mkMintedLiquidity $ mintedVal rtb
        , mkAnyTappedLiquidity $ BBS.mkBufferLiquidity $ depositVal rtb
        ]

    liquidityToRTB uliq = SimpleRealtimeBalance uliq def def def

    untypedLiquidityVectorToRTB (UntypedLiquidityVector uliq uvec) = assert (length uvec == 3) $
        SimpleRealtimeBalance uliq (head uvec) (uvec!!1) (uvec!!2)

    typedLiquidityVectorToRTB (TypedLiquidityVector (UntappedLiquidity uliq) tvec) =
        SimpleRealtimeBalance uliq mliq d od
        where d = foldr ((+) . (`fromAnyTappedLiquidity` BBS.bufferLiquidityTag)) def tvec
              mliq = foldr ((+) . (`fromAnyTappedLiquidity` TBA.mintedLiquidityTag)) def tvec
              od = def

-- ============================================================================
-- SimpleAddress Type
--
-- Note: It must consist of only alphabetical letters
--
newtype SimpleAddress = SimpleAddress String
    deriving newtype (Eq, Ord, Binary, Show)
    deriving anyclass (Address)

-- SimpleAddress public constructor
createSimpleAddress :: String -> Maybe SimpleAddress
createSimpleAddress a = if isValidAddress a then Just $ SimpleAddress a else Nothing
    where isValidAddress = all (\x -> any ($ x) [isAlpha, (== '_')])

instance IsString SimpleAddress where
    fromString = fromJust . createSimpleAddress

data SimpleSuperfluidTypes

instance SuperfluidTypes SimpleSuperfluidTypes where
    type SFT_FLOAT SimpleSuperfluidTypes = SFDouble
    type SFT_LQ SimpleSuperfluidTypes = Wad
    type SFT_TS SimpleSuperfluidTypes = SimpleTimestamp
    type SFT_RTB SimpleSuperfluidTypes = SimpleRealtimeBalance
    type SFT_ADDR SimpleSuperfluidTypes = SimpleAddress

-- ============================================================================
-- Agreement Types
--
instance Show (TBA.TBAAccountData SimpleSuperfluidTypes) where
    show x = printf "{ t = %s, uliq = %s, mliq = %s }"
        (show $ TBA.settledAt x)
        (show $ TBA.untappedLiquidity x)
        (show $ TBA.mintedLiquidity x)

instance Show (CFA.CFAContractData SimpleSuperfluidTypes) where
    show x = printf "{ flowLastUpdatedAt = %s, flowRate = %s, flowBuffer = %s }"
        (show $ CFA.flowLastUpdatedAt x) (show $ CFA.flowRate x) (show $ CFA.flowBuffer x)

instance Show (CFA.CFAAccountData SimpleSuperfluidTypes) where
    show x = printf "{ t = %s, uliq = %s, buf = %s, fr = %s }"
        (show $ CFA.settledAt x)
        (show $ CFA.settledUntappedLiquidity x)
        (show $ CFA.settledBufferLiquidity x)
        (show $ CFA.netFlowRate x)

instance Show (DFA.DFAContractData SimpleSuperfluidTypes) where
    show x = printf "{ t_u = %s, δ = %s, λ = %s }"
        (show $ DFA.flowLastUpdatedAt x) (show $ DFA.distributionLimit x) (show $ DFA.decayingFactor x)

instance Show (DFA.DFAAccountData SimpleSuperfluidTypes) where
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
    forall a. ( Agreement a
              , DistributionForAgreement a ~ SimpleSuperfluidTypes
              , Show (AgreementAccountData a)
              )
    => MkSimpleAgreementAccountData (AgreementAccountData a)

instance Show AnySimpleAgreementAccountData where
    show (MkSimpleAgreementAccountData a) = show a
