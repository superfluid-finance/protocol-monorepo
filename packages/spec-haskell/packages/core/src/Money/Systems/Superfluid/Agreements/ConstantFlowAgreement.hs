{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.Agreements.ConstantFlowAgreement
    ( AgreementContractData (..)
    , AgreementAccountData (..)
    , AgreementParties (..)
    , CFAContractData
    , CFAAccountData
    ) where

import           Data.Default                                            (Default (..))
import           Data.Kind                                               (Type)
import           Data.Type.TaggedTypeable                                (TaggedTypeable (..))

import           Money.Systems.Superfluid.Concepts.Agreement
    ( Agreement (..)
    , AgreementAccountData (..)
    , AgreementContractData
    )
import           Money.Systems.Superfluid.Concepts.Liquidity
    ( Liquidity
    , UntappedLiquidity (..)
    , mkAnyTappedLiquidity
    , untypeLiquidity
    )
import           Money.Systems.Superfluid.Concepts.RealtimeBalance
    ( RealtimeBalance (..)
    , TypedLiquidityVector (..)
    )
import           Money.Systems.Superfluid.Concepts.SuperfluidTypes
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency as BBS


type CFA :: Type -> Type
data CFA sft
type CFAContractData sft = AgreementContractData (CFA sft)
type CFAAccountData sft = AgreementAccountData (CFA sft)
instance SuperfluidTypes sft => Agreement (CFA sft) where
    type DistributionForAgreement (CFA sft) = sft
    data AgreementContractData (CFA sft) = CFAContractData
        { flowLastUpdatedAt :: SFT_TS sft
        , flowRate          :: SFT_LQ sft
        , flowBuffer        :: BBS.BufferLiquidity (SFT_LQ sft)
        }
    data AgreementAccountData (CFA sft) = CFAAccountData
        { settledAt                :: SFT_TS sft
        , settledUntappedLiquidity :: UntappedLiquidity (SFT_LQ sft)
        , settledBufferLiquidity   :: BBS.BufferLiquidity (SFT_LQ sft)
        , netFlowRate              :: SFT_LQ sft
        }
    data AgreementParties (CFA sft) = CFAParties (CFAAccountData sft) (CFAAccountData sft)

    providedBalanceByAgreement CFAAccountData
        { settledAt = t_s
        , settledUntappedLiquidity = (UntappedLiquidity uliq_s)
        , settledBufferLiquidity = buf_s
        , netFlowRate = fr
        } t =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
            ( UntappedLiquidity $ uliq_s + calc_liquidity_delta fr t_s t )
            [ mkAnyTappedLiquidity buf_s ]

    createAgreementParties old new = CFAParties
        CFAAccountData
        { settledAt = t'
        , netFlowRate = negate flowRateDelta
        , settledUntappedLiquidity = UntappedLiquidity $ negate flowPeriodDelta - untypeLiquidity flowBufferDelta
        , settledBufferLiquidity = flowBufferDelta
        }
        CFAAccountData
        { settledAt = t'
        , netFlowRate = flowRateDelta
        , settledUntappedLiquidity = UntappedLiquidity $ flowPeriodDelta
        , settledBufferLiquidity = def
        }
        where
            fr = flowRate old
            t = flowLastUpdatedAt old
            t' = flowLastUpdatedAt new
            flowPeriodDelta = calc_liquidity_delta fr t t'
            flowRateDelta = flowRate new - fr
            flowBufferDelta = flowBuffer new - flowBuffer old

    liftAgreementParties2 f (CFAParties s r) (CFAParties s' r') = CFAParties (f s s') (f r r')

instance SuperfluidTypes sft => Default (CFAContractData sft) where
    def = CFAContractData
        { flowLastUpdatedAt = def
        , flowRate = def
        , flowBuffer = def
        }
instance SuperfluidTypes sft => TaggedTypeable (CFAContractData sft) where tagFromProxy _ = "CFA#"


instance SuperfluidTypes sft => Semigroup (CFAAccountData sft) where
    (<>) a b = CFAAccountData
               { settledAt = settledAt b
               , settledUntappedLiquidity = settledUntappedLiquidity a + settledUntappedLiquidity b
               , netFlowRate = netFlowRate a + netFlowRate b
               , settledBufferLiquidity = settledBufferLiquidity a + settledBufferLiquidity b
               }
instance SuperfluidTypes sft => Monoid (CFAAccountData sft) where
    mempty = CFAAccountData
        { settledAt = def
        , settledUntappedLiquidity = def
        , settledBufferLiquidity = def
        , netFlowRate = def }

instance SuperfluidTypes sft => TaggedTypeable (CFAAccountData sft) where tagFromProxy _ = "CFA"

-- ============================================================================
-- Internal functions
--
-- Calculate liquidity delta for settlement
calc_liquidity_delta :: (Liquidity lq, Timestamp ts) => lq -> ts -> ts -> lq
calc_liquidity_delta fr t0 t1 = fr * fromIntegral (t1 - t0)
