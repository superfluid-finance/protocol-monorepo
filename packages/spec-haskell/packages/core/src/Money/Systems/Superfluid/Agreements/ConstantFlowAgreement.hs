{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Money.Systems.Superfluid.Agreements.ConstantFlowAgreement
    ( AgreementContractData' (..)
    , AgreementAccountData' (..)
    , AgreementParties (..)
    , CFAContractData
    , CFAAccountData
    ) where

import           Data.Default                                            (Default (..))
import           Data.Internal.TaggedTypeable                            (TaggedTypeable (..))
import           Data.Kind                                               (Type)
import           Text.Printf                                             (printf)

--
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


type ConstantFlowAgreement :: Type -> Type
data ConstantFlowAgreement sft
type CFAContractData sft = AgreementContractData' (ConstantFlowAgreement sft)
type CFAAccountData sft = AgreementAccountData' (ConstantFlowAgreement sft)
instance SuperfluidTypes sft => Agreement (ConstantFlowAgreement sft) where
    type DistributionForAgreement (ConstantFlowAgreement sft) = sft
    data AgreementContractData' (ConstantFlowAgreement sft) = CFAContractData
        { flowLastUpdatedAt :: SFT_TS sft
        , flowRate          :: SFT_LQ sft
        , flowBuffer        :: BBS.BufferLiquidity (SFT_LQ sft)
        }
    data AgreementAccountData' (ConstantFlowAgreement sft) = CFAAccountData
        { settledAt                :: SFT_TS sft
        , settledUntappedLiquidity :: UntappedLiquidity (SFT_LQ sft)
        , settledBufferLiquidity   :: BBS.BufferLiquidity (SFT_LQ sft)
        , netFlowRate              :: SFT_LQ sft
        }
    data AgreementParties (ConstantFlowAgreement sft) = CFAParties (CFAAccountData sft) (CFAAccountData sft)

    createAgreementParties old new = CFAParties
        CFAAccountData
        { settledAt = t1
        , netFlowRate = negate flowRateDelta
        , settledUntappedLiquidity = UntappedLiquidity $ negate flowPeriodDelta - untypeLiquidity flowBufferDelta
        , settledBufferLiquidity = flowBufferDelta
        }
        CFAAccountData
        { settledAt = t1
        , netFlowRate = flowRateDelta
        , settledUntappedLiquidity = UntappedLiquidity $ flowPeriodDelta
        , settledBufferLiquidity = def
        }
        where
            fr = flowRate old
            t0 = flowLastUpdatedAt old
            t1 = flowLastUpdatedAt new
            flowPeriodDelta = calc_liquidity_delta fr t0 t1
            flowRateDelta = flowRate new - fr
            flowBufferDelta = flowBuffer new - flowBuffer old

    liftAgreementParties2 f (CFAParties s r) (CFAParties s' r') = CFAParties (f s  s') (f r r')

instance SuperfluidTypes sft => Semigroup (CFAAccountData sft) where
    (<>) a b = CFAAccountData
               { settledAt = settledAt b
               , settledUntappedLiquidity = settledUntappedLiquidity a + settledUntappedLiquidity b
               , settledBufferLiquidity = settledBufferLiquidity a + settledBufferLiquidity b
               , netFlowRate = netFlowRate a + netFlowRate b
               }

instance SuperfluidTypes sft => Monoid (CFAAccountData sft) where
    mempty = CFAAccountData
        { settledAt = def
        , settledUntappedLiquidity = def
        , settledBufferLiquidity = def
        , netFlowRate = def }

-- | CFAAccountData Type (is AgreementAccountData)
--
instance SuperfluidTypes sft => TaggedTypeable (CFAAccountData sft) where tagFromProxy _ = "CFA"

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

-- | CFAContractData Type
--
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

instance SuperfluidTypes sft => AgreementContractData (CFAContractData sft) sft (CFAAccountData sft)

instance SuperfluidTypes sft => Show (CFAAccountData sft) where
    show x = printf "{ t = %s, uliq = %s, buf = %s, fr = %s }"
        (show $ settledAt x)
        (show $ settledUntappedLiquidity x)
        (show $ settledBufferLiquidity x)
        (show $ netFlowRate x)

-- ============================================================================
-- Internal functions
--
-- Calculate liquidity delta for settlement
calc_liquidity_delta :: (Liquidity lq, Timestamp ts) => lq -> ts -> ts -> lq
calc_liquidity_delta fr t0 t1 = fr * fromIntegral (t1 - t0)
