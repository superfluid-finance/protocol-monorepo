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
data CFA sfd
type CFAContractData sfd = AgreementContractData (CFA sfd)
type CFAAccountData sfd = AgreementAccountData (CFA sfd)
type CFAPartiesF sfd = AgreementPartiesF (CFA sfd)
type CFAParties sfd = (CFAPartiesF sfd) (CFAAccountData sfd)
instance SuperfluidDistribution sfd => Agreement (CFA sfd) where
    type DistributionForAgreement (CFA sfd) = sfd

    data AgreementContractData (CFA sfd) = CFAContractData
        { flowLastUpdatedAt :: SFT_TS sfd
        , flowRate          :: SFT_LQ sfd
        , flowBuffer        :: BBS.BufferLiquidity (SFT_LQ sfd)
        }

    data AgreementAccountData (CFA sfd) = CFAAccountData
        { settledAt                :: SFT_TS sfd
        , settledUntappedLiquidity :: UntappedLiquidity (SFT_LQ sfd)
        , settledBufferLiquidity   :: BBS.BufferLiquidity (SFT_LQ sfd)
        , netFlowRate              :: SFT_LQ sfd
        }

    data AgreementPartiesF (CFA sfd) a = CFAPartiesF a a deriving stock (Functor)

    data AgreementOperation (CFA sfd) =
        -- flowRate, newFlowBuffer, t'
        Updatelow (SFT_LQ sfd) (BBS.BufferLiquidity (SFT_LQ sfd)) (SFT_TS sfd)

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

instance SuperfluidDistribution sfd => Applicative (CFAPartiesF sfd) where
    pure a = CFAPartiesF a a
    liftA2 f (CFAPartiesF s r) (CFAPartiesF s' r') = CFAPartiesF (f s s') (f r r')

instance SuperfluidDistribution sfd => TaggedTypeable (CFAContractData sfd) where tagFromProxy _ = "CFA#"
instance SuperfluidDistribution sfd => Default (CFAContractData sfd) where
    def = CFAContractData
        { flowLastUpdatedAt = def
        , flowRate = def
        , flowBuffer = def
        }

instance SuperfluidDistribution sfd => TaggedTypeable (CFAAccountData sfd) where tagFromProxy _ = "CFA"
instance SuperfluidDistribution sfd => Default (CFAAccountData sfd) where
    def = CFAAccountData
        { settledAt = def
        , settledUntappedLiquidity = def
        , settledBufferLiquidity = def
        , netFlowRate = def
        }
instance SuperfluidDistribution sfd => Semigroup (CFAAccountData sfd) where
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
