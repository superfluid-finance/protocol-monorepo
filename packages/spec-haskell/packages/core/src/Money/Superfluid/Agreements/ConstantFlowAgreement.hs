{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Money.Superfluid.Agreements.ConstantFlowAgreement
    ( CFAContractData (..)
    , CFAAccountData (..)
    , ConstantFlow (..)
    , updateFlow
    ) where

import           Data.Default                                    (Default (..))
import           Data.Internal.TaggedTypeable
import           Text.Printf                                     (printf)

import           Money.Distribution.Concepts                     (Timestamp)
--
import           Money.Superfluid.Concepts.Agreement             (AgreementAccountData (..), AgreementContractData)
import           Money.Superfluid.Concepts.Liquidity
    ( Liquidity
    , UntappedLiquidity (..)
    , mkAnyTappedLiquidity
    , untypeLiquidity
    )
import           Money.Superfluid.Concepts.RealtimeBalance       (RealtimeBalance (..), TypedLiquidityVector (..))
import           Money.Superfluid.Concepts.SuperfluidTypes
--
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency as BBS


-- ============================================================================
-- | CFAContractData Type
--
data CFAContractData sft = CFAContractData
    { flowLastUpdatedAt :: SFT_TS sft
    , flowRate          :: SFT_LQ sft
    , flowBuffer        :: BBS.BufferLiquidity (SFT_LQ sft)
    }
instance SuperfluidTypes sft => TaggedTypeable (CFAContractData sft) where tagFromProxy _ = "CFA#"
instance SuperfluidTypes sft => Default (CFAContractData sft) where
    def = CFAContractData
        { flowLastUpdatedAt = def
        , flowRate = def
        , flowBuffer = def
        }

instance SuperfluidTypes sft => Show (CFAContractData sft) where
    show x = printf "{ flowLastUpdatedAt = %s, flowRate = %s, flowBuffer = %s }"
        (show $ flowLastUpdatedAt x) (show $ flowRate x) (show $ flowBuffer x)

instance SuperfluidTypes sft => AgreementContractData (CFAContractData sft) sft

-- ============================================================================
-- | CFAAccountData Type (is AgreementAccountData)
--
data CFAAccountData sft = CFAAccountData
    { settledAt                :: SFT_TS sft
    , settledUntappedLiquidity :: UntappedLiquidity (SFT_LQ sft)
    , settledBufferLiquidity   :: BBS.BufferLiquidity (SFT_LQ sft)
    , netFlowRate              :: SFT_LQ sft
    }
instance SuperfluidTypes sft => TaggedTypeable (CFAAccountData sft) where tagFromProxy _ = "CFA"
instance SuperfluidTypes sft => Default (CFAAccountData sft) where
    def = CFAAccountData
        { settledAt = def
        , settledUntappedLiquidity = def
        , settledBufferLiquidity = def
        , netFlowRate = def }

calc_liquidity_delta :: (Liquidity lq, Timestamp ts) => lq -> ts -> ts -> lq
calc_liquidity_delta fr t0 t1 = fr * fromIntegral (t1 - t0)

instance SuperfluidTypes sft => AgreementAccountData (CFAAccountData sft) sft where
    providedBalanceOfAgreement CFAAccountData
        { settledAt = t_s
        , settledUntappedLiquidity = (UntappedLiquidity uliq_s)
        , settledBufferLiquidity = buf_s
        , netFlowRate = fr
        } t =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
            ( UntappedLiquidity $ uliq_s + calc_liquidity_delta fr t_s t )
            [ mkAnyTappedLiquidity buf_s ]

instance SuperfluidTypes sft => Show (CFAAccountData sft) where
    show x = printf "{ t = %s, uliq = %s, buf = %s, fr = %s }"
        (show $ settledAt x)
        (show $ settledUntappedLiquidity x)
        (show $ settledBufferLiquidity x)
        (show $ netFlowRate x)

-- ============================================================================
-- CFA Operations
--
data ConstantFlow sft = ConstantFlow
    { flowContract :: CFAContractData sft
    , flowSender   :: CFAAccountData sft
    , flowReceiver :: CFAAccountData sft
    }

updateFlow
    :: SuperfluidTypes sft
    => ConstantFlow sft -> SFT_LQ sft -> BBS.BufferLiquidity (SFT_LQ sft) -> SFT_TS sft -> ConstantFlow sft
updateFlow (ConstantFlow cfaACD senderAAD receiverAAD) newFlowRate newFlowBuffer t =
    ConstantFlow CFAContractData
            { flowLastUpdatedAt = t
            , flowRate = newFlowRate
            , flowBuffer = newFlowBuffer
            }
        ( update_flow_aad senderAAD (negate flowRateDelta) )
        ( update_flow_aad receiverAAD flowRateDelta )
    where
    flowRateDelta = newFlowRate - flowRate cfaACD
    flowBufferDelta = newFlowBuffer -  flowBuffer cfaACD
    update_flow_aad CFAAccountData
        { netFlowRate = fr
        , settledUntappedLiquidity = (UntappedLiquidity uliq_s)
        , settledBufferLiquidity = buf_s
        , settledAt = t_s
        } fr_delta
        = CFAAccountData
        { netFlowRate = fr + fr_delta
        , settledUntappedLiquidity = UntappedLiquidity $
            uliq_s + calc_liquidity_delta fr t_s t - untypeLiquidity flowBufferDelta
        , settledBufferLiquidity = buf_s + flowBufferDelta
        , settledAt = t
        }
