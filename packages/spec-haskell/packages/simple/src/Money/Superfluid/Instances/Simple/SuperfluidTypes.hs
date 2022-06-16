{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Money.Superfluid.Instances.Simple.SuperfluidTypes
    ( module Money.Superfluid.Concepts.Liquidity
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
    , module Money.Superfluid.Concepts.RealtimeBalance
    , SimpleRealtimeBalance (..)
    -- SimpleSuperfluidTypes
    , SimpleSuperfluidTypes
    ) where

import           Control.Exception                                        (assert)
import           Data.Binary
import           Data.Char                                                (isAlpha)
import           Data.Default
import           Data.Maybe
import           Data.String
import           GHC.Generics                                             (Generic)
import           Text.Printf                                              (printf)

import           Money.Distribution.Concepts                              (Timestamp)
--
import           Money.Superfluid.Concepts.Liquidity
import           Money.Superfluid.Concepts.RealtimeBalance
import           Money.Superfluid.Concepts.SuperfluidTypes
--
import qualified Money.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency          as BBS


-- | SFDouble Type
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
        where d = foldr ((+) . (`untypeLiquidityOfType` BBS.bufferLiquidityTag)) def tvec
              mliq = foldr ((+) . (`untypeLiquidityOfType` TBA.mintedLiquidityTag)) def tvec
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
