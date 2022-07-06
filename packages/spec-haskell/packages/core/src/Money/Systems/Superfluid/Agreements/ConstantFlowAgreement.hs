{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.Agreements.ConstantFlowAgreement
    ( AgreementContractData (..)
    , AgreementAccountData (..)
    , AgreementContractPartiesF (..)
    , AgreementOperation (..)
    , ContractData
    , AccountData
    , ContractPartiesF
    , ContractParties
    ) where

import           Control.Applicative                                     (Applicative (..))
import           Data.Coerce                                             (coerce)
import           Data.Default                                            (Default (..))
import           Data.Kind                                               (Type)
import           Data.Type.TaggedTypeable                                (TaggedTypeable (..))

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency as BBS


-- Agreement Definition
--
type CFA :: Type -> Type -- kind signature is required to make GHC happy
data CFA sft

instance SuperfluidTypes sft => Agreement (CFA sft) sft where
    data AgreementContractData (CFA sft) = ContractData
        { flowLastUpdatedAt :: SFT_TS sft
        , flowRate          :: SFT_LQ sft
        , flowBuffer        :: BBS.BufferLiquidity (SFT_LQ sft)
        }

    data AgreementAccountData (CFA sft) = AccountData
        { settledAt                :: SFT_TS sft
        , settledUntappedLiquidity :: UntappedLiquidity (SFT_LQ sft)
        , settledBufferLiquidity   :: BBS.BufferLiquidity (SFT_LQ sft)
        , netFlowRate              :: SFT_LQ sft
        }

    data AgreementContractPartiesF (CFA sft) a = ContractPartiesF
        { flowSender   :: a
        , flowReceiver :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (CFA sft) =
        -- flowRate, newFlowBuffer, t'
        UpdateFlow (SFT_LQ sft) (BBS.BufferLiquidity (SFT_LQ sft)) (SFT_TS sft)

    balanceProvidedByAgreement AccountData
        { settledAt = t_s
        , settledUntappedLiquidity = (UntappedLiquidity uliq_s)
        , settledBufferLiquidity = buf_s
        , netFlowRate = fr
        } t =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
            ( UntappedLiquidity $ uliq_s + calc_liquidity_delta fr t_s t )
            [ mkAnyTappedLiquidity buf_s ]

    applyAgreementOperation acd (UpdateFlow newFlowRate newFlowBuffer t') = let
        acd' = acd { flowRate = newFlowRate, flowBuffer = newFlowBuffer, flowLastUpdatedAt = t' }
        acps' = ContractPartiesF AccountData
                           { settledAt = t'
                           , netFlowRate = negate flowRateDelta
                           , settledUntappedLiquidity = UntappedLiquidity $ negate flowPeriodDelta - coerce flowBufferDelta
                           , settledBufferLiquidity = flowBufferDelta
                           }
                           AccountData
                           { settledAt = t'
                           , netFlowRate = flowRateDelta
                           , settledUntappedLiquidity = UntappedLiquidity $ flowPeriodDelta
                           , settledBufferLiquidity = def
                           }
        in (acd', acps')
        where
            fr = flowRate acd
            t = flowLastUpdatedAt acd
            flowPeriodDelta = calc_liquidity_delta fr t t'
            flowRateDelta = newFlowRate - fr
            flowBufferDelta = newFlowBuffer - flowBuffer acd

type ContractData sft = AgreementContractData (CFA sft)
type AccountData sft = AgreementAccountData (CFA sft)
type ContractPartiesF sft = AgreementContractPartiesF (CFA sft)
type ContractParties sft = (ContractPartiesF sft) (AccountData sft)

instance SuperfluidTypes sft => Applicative (ContractPartiesF sft) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')

instance SuperfluidTypes sft => TaggedTypeable (ContractData sft) where tagFromProxy _ = "CFA#"
instance SuperfluidTypes sft => Default (ContractData sft) where
    def = ContractData
        { flowLastUpdatedAt = def
        , flowRate = def
        , flowBuffer = def
        }

instance SuperfluidTypes sft => TaggedTypeable (AccountData sft) where tagFromProxy _ = "CFA"
instance SuperfluidTypes sft => Default (AccountData sft) where
    def = AccountData
        { settledAt = def
        , settledUntappedLiquidity = def
        , settledBufferLiquidity = def
        , netFlowRate = def
        }
instance SuperfluidTypes sft => Semigroup (AccountData sft) where
    (<>) a b = AccountData
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
