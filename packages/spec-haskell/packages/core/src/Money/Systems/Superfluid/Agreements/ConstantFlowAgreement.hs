{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.Agreements.ConstantFlowAgreement
    ( AgreementContractData (..)
    , AgreementAccountData (..)
    , AgreementPartiesF (..)
    , AgreementOperation (..)
    , CFAContractData
    , CFAAccountData
    , CFAPartiesF
    , CFAParties
    ) where

import           Control.Applicative                                     (Applicative (..))
import           Data.Coerce                                             (coerce)
import           Data.Default                                            (Default (..))
import           Data.Kind                                               (Type)
import           Data.Type.TaggedTypeable                                (TaggedTypeable (..))

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency as BBS


type CFA :: Type -> Type
data CFA sft
type CFAContractData sft = AgreementContractData (CFA sft)
type CFAAccountData sft = AgreementAccountData (CFA sft)
type CFAPartiesF sft = AgreementPartiesF (CFA sft)
type CFAParties sft = (CFAPartiesF sft) (CFAAccountData sft)
instance SuperfluidDistribution sft => Agreement (CFA sft) where
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

    data AgreementPartiesF (CFA sft) a = CFAPartiesF a a deriving stock (Functor)

    data AgreementOperation (CFA sft) =
        -- flowRate, newFlowBuffer, t'
        Updatelow (SFT_LQ sft) (BBS.BufferLiquidity (SFT_LQ sft)) (SFT_TS sft)

    providedBalanceByAgreement CFAAccountData
        { settledAt = t_s
        , settledUntappedLiquidity = (UntappedLiquidity uliq_s)
        , settledBufferLiquidity = buf_s
        , netFlowRate = fr
        } t =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
            ( UntappedLiquidity $ uliq_s + calc_liquidity_delta fr t_s t )
            [ mkAnyTappedLiquidity buf_s ]

    createAgreementPartiesDelta acd (Updatelow newFlowRate newFlowBuffer t') = let
        acd' = acd { flowRate = newFlowRate, flowBuffer = newFlowBuffer, flowLastUpdatedAt = t' }
        aps' = CFAPartiesF CFAAccountData
                           { settledAt = t'
                           , netFlowRate = negate flowRateDelta
                           , settledUntappedLiquidity = UntappedLiquidity $ negate flowPeriodDelta - coerce flowBufferDelta
                           , settledBufferLiquidity = flowBufferDelta
                           }
                           CFAAccountData
                           { settledAt = t'
                           , netFlowRate = flowRateDelta
                           , settledUntappedLiquidity = UntappedLiquidity $ flowPeriodDelta
                           , settledBufferLiquidity = def
                           }
        in (acd', aps')
        where
            fr = flowRate acd
            t = flowLastUpdatedAt acd
            flowPeriodDelta = calc_liquidity_delta fr t t'
            flowRateDelta = newFlowRate - fr
            flowBufferDelta = newFlowBuffer - flowBuffer acd

instance SuperfluidDistribution sft => Applicative (CFAPartiesF sft) where
    pure a = CFAPartiesF a a
    liftA2 f (CFAPartiesF s r) (CFAPartiesF s' r') = CFAPartiesF (f s s') (f r r')

instance SuperfluidDistribution sft => TaggedTypeable (CFAContractData sft) where tagFromProxy _ = "CFA#"
instance SuperfluidDistribution sft => Default (CFAContractData sft) where
    def = CFAContractData
        { flowLastUpdatedAt = def
        , flowRate = def
        , flowBuffer = def
        }

instance SuperfluidDistribution sft => TaggedTypeable (CFAAccountData sft) where tagFromProxy _ = "CFA"
instance SuperfluidDistribution sft => Default (CFAAccountData sft) where
    def = CFAAccountData
        { settledAt = def
        , settledUntappedLiquidity = def
        , settledBufferLiquidity = def
        , netFlowRate = def
        }
instance SuperfluidDistribution sft => Semigroup (CFAAccountData sft) where
    (<>) a b = CFAAccountData
               { settledAt = settledAt b
               , settledUntappedLiquidity = settledUntappedLiquidity a + settledUntappedLiquidity b
               , netFlowRate = netFlowRate a + netFlowRate b
               , settledBufferLiquidity = settledBufferLiquidity a + settledBufferLiquidity b
               }

-- ============================================================================
-- Internal functions
--
-- Calculate liquidity delta for settlement
calc_liquidity_delta :: (Liquidity lq, Timestamp ts) => lq -> ts -> ts -> lq
calc_liquidity_delta fr t0 t1 = fr * fromIntegral (t1 - t0)
