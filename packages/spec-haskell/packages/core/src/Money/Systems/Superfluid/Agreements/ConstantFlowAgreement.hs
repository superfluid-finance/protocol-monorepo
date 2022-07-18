{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Agreements.ConstantFlowAgreement
    ( MonetaryUnitLens (..)
    , MonetaryUnitData (..)
    , ContractLens (..)
    , ContractData (..)
    , AgreementContractPartiesF (..)
    , FlowRate
    , AgreementOperation (..)
    , ContractPartiesF
    , ContractPartiesMUD
    ) where

import           Control.Applicative                                     (Applicative (..))
import           Data.Coerce                                             (coerce)
import           Data.Default                                            (Default (..))
import           Data.Kind                                               (Type)
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency as BBS


-- * CFA.MonetaryUnitData
--

class (Default amudL, SuperfluidTypes sft) => MonetaryUnitLens amudL sft | amudL -> sft where
    settledAt            :: Lens' amudL (SFT_TS sft)
    settledUntappedValue :: Lens' amudL (UntappedValue (SFT_MVAL sft))
    settledBufferValue   :: Lens' amudL (BBS.BufferValue (SFT_MVAL sft))
    netFlowRate          :: Lens' amudL (SFT_MVAL sft)

type MonetaryUnitData :: Type -> Type -> Type -- kind signature is required to make GHC happy
newtype MonetaryUnitData amudL sft = MkMonetaryUnitData amudL

instance MonetaryUnitLens amudL sft => Semigroup (MonetaryUnitData amudL sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & set  settledAt            (  b^.settledAt)
                  & over settledUntappedValue (+ b^.settledUntappedValue)
                  & over netFlowRate          (+ b^.netFlowRate)
                  & over settledBufferValue   (+ b^.settledBufferValue)
        in MkMonetaryUnitData c
instance MonetaryUnitLens amudL sft => Monoid (MonetaryUnitData amudL sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLens amudL sft => AgreementMonetaryUnitData (MonetaryUnitData amudL sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) t = typedValuesToRTB
            ( UntappedValue $ uval_s + calc_value_delta fr t_s t )
            [ mkAnyTappedValue buf_s ]
        where t_s                  = a^.settledAt
              UntappedValue uval_s = a^.settledUntappedValue
              buf_s                = a^.settledBufferValue
              fr                   = a^.netFlowRate

-- * ITA.ContractData
--

class (Default cdL, SuperfluidTypes sft) => ContractLens cdL sft | cdL -> sft where
    flowLastUpdatedAt :: Lens' cdL (SFT_TS sft)
    flowRate          :: Lens' cdL (SFT_MVAL sft)
    flowBuffer        :: Lens' cdL (BBS.BufferValue (SFT_MVAL sft))

type ContractData :: Type -> Type -> Type -> Type
newtype ContractData cdL amudL sft = MkContractData cdL

instance ContractLens cdL sft => Default (ContractData cdL amudL sft) where def = MkContractData def

type FlowRate sft = SFT_MVAL sft

instance ( ContractLens cdL sft
         , MonetaryUnitLens amudL sft
         , AgreementMonetaryUnitData (MonetaryUnitData amudL sft) sft
         ) => AgreementContractData (ContractData cdL amudL sft) (MonetaryUnitData amudL sft) sft where

    data AgreementContractPartiesF (ContractData cdL amudL sft) a = ContractPartiesF
        { flowSender   :: a
        , flowReceiver :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (ContractData cdL amudL sft) =
        --         flowRate       newFlowBuffer
        UpdateFlow (FlowRate sft) (BBS.BufferValue (SFT_MVAL sft))

    applyAgreementOperation (MkContractData acd) (UpdateFlow newFlowRate newFlowBuffer) t' = let
        acd'  = acd & set flowRate newFlowRate
                    & set flowBuffer newFlowBuffer
                    & set flowLastUpdatedAt t'
        acpsΔ = fmap MkMonetaryUnitData (ContractPartiesF
                    (def & set settledAt t'
                         & set netFlowRate (- flowRateDelta)
                         & set settledUntappedValue (UntappedValue $ (- flowPeriodDelta) - coerce flowBufferDelta)
                         & set settledBufferValue flowBufferDelta)
                    (def & set settledAt t'
                         & set netFlowRate flowRateDelta
                         & set settledUntappedValue (UntappedValue flowPeriodDelta)
                         & set settledBufferValue def))
        in (MkContractData acd', acpsΔ)
        where
            t               = acd^.flowLastUpdatedAt
            fr              = acd^.flowRate
            flowPeriodDelta = calc_value_delta fr t t'
            flowRateDelta   = newFlowRate - fr
            flowBufferDelta = newFlowBuffer - acd^.flowBuffer

type ContractPartiesF   sft cdL amudL = AgreementContractPartiesF (ContractData cdL amudL sft)
type ContractPartiesMUD sft cdL amudL = ContractPartiesF sft cdL (MonetaryUnitData amudL sft)

instance Applicative (ContractPartiesF sft cdL amudL) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')

-- ============================================================================
-- Internal functions
--
-- Calculate value delta for settlement
calc_value_delta :: (Value v, Timestamp ts) => v -> ts -> ts -> v
calc_value_delta fr t0 t1 = fr * fromIntegral (t1 - t0)
