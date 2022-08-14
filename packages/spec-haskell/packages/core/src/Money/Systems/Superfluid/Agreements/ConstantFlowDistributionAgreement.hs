{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Instant distribution agreement.
--
-- It is instant transferring over an proportional distribution index
--
-- This module is typically imported using qualified name CFDA.
module Money.Systems.Superfluid.Agreements.ConstantFlowDistributionAgreement where

import           Data.Default
import           GHC.Generics

import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes
--
import           Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionCommon
import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow                     as CFMUD


-- * Contracts

data DistributionContract sft = DistributionContract
    { dc_updated_at     :: SFT_TS sft
    , dc_flow_rate      :: SFT_MVAL sft
    , dc_value_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (DistributionContract sft)

data SubscriptionContract sft = SubscriptionContract
    { sc_settled_value          :: UntappedValue (SFT_MVAL sft)
    , sc_settled_value_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (SubscriptionContract sft)

type DistributionContractFull sft = (DistributionContractBase sft, DistributionContract sft)

type SubscriptionContractFull sft = (SubscriptionContractBase sft, SubscriptionContract sft)

type PublisherContract sft = DistributionContractFull sft

type SubscriberContract sft = (DistributionContractFull sft , SubscriptionContractFull sft)

-- * Publisher Monetary unit data

data PublisherData sft = PublisherData
    { pub_settled_at      :: SFT_TS sft
    , pub_settled_value   :: UntappedValue (SFT_MVAL sft)
    , pub_total_flow_rate :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (PublisherData sft)

type PublisherMonetaryUnitData sft = CFMUD.MonetaryUnitData (PublisherData sft) sft
instance SuperfluidSystemTypes sft => SemigroupMonetaryUnitData (PublisherMonetaryUnitData sft) sft

-- * Subscriber Monetary unit data

type SubscriberData sft = SubscriberContract sft
type SubscriberMonetaryUnitData sft = CFMUD.MonetaryUnitData (SubscriberData sft) sft

instance SuperfluidSystemTypes sft => CFMUD.MonetaryUnitLenses (PublisherData sft) sft where
    settledAt          = $(field 'pub_settled_at)
    settledValue       = $(field 'pub_settled_value)
    netFlowRate        = $(field 'pub_total_flow_rate)

instance SuperfluidSystemTypes sft => CFMUD.MonetaryUnitLenses (SubscriberData sft) sft where
    settledAt     = readOnlyLens
        (\((_ , _),
           (SubscriptionContractBase { sub_settled_at = t }
           , _)) -> t)

    netFlowRate   = readOnlyLens
        (\(( DistributionContractBase { total_unit     = tu }
           , DistributionContract { dc_flow_rate       = dcfr }),
           ( SubscriptionContractBase { sub_owned_unit = u }
           , _)) -> floor $ fromIntegral dcfr * u / tu )

    settledValue = readOnlyLens
        (\(( DistributionContractBase { total_unit            = tu }
           , DistributionContract { dc_value_per_unit         = vpu
                                  , dc_flow_rate              = dcfr
                                  , dc_updated_at             = t_dc }),
           ( SubscriptionContractBase { sub_owned_unit        = u
                                      , sub_settled_at        = t_sc
                                      }
           , SubscriptionContract { sc_settled_value          = UntappedValue sv
                                  , sc_settled_value_per_unit = svpu
                                  })
          ) -> let vpuΔ = floor $ fromIntegral dcfr * fromIntegral (t_dc - t_sc) / tu
               in  UntappedValue $ sv + floor (u * fromIntegral (vpu - svpu - vpuΔ)))

-- * Publisher Operations

instance SuperfluidSystemTypes sft => MonetaryUnitDataClass (PublisherContract sft) sft where

instance SuperfluidSystemTypes sft => AgreementContract (PublisherContract sft) sft where
    applyAgreementOperation (dcBase, dc) (UpdateDistributionFlowRate dcfr') t' = let
        -- FIXME can be a black hole due to tu == 0 or precision error
        --       need to communicate the actual distribution flow rate instead
        settledΔ = dcfr * fromIntegral (t' - t_dc)
        vpuΔ = if tu /= 0 then floor $ fromIntegral settledΔ / tu else 0

        dc' = dc { dc_updated_at = t'
                 , dc_value_per_unit = vpu + vpuΔ
                 , dc_flow_rate = dcfr'
                 }

        mudsΔ = PublisherOperationOutputF
                (def & set CFMUD.settledAt t'
                     & set CFMUD.netFlowRate (dcfr - dcfr') -- reverse sign for outgoing flow
                     & set CFMUD.settledValue def)

        in ((dcBase, dc'), fmap CFMUD.MkMonetaryUnitData mudsΔ)

        where DistributionContractBase { total_unit    = tu
                                       } = dcBase
              DistributionContract { dc_updated_at     = t_dc
                                   , dc_value_per_unit = vpu
                                   , dc_flow_rate      = dcfr
                                   } = dc

    concatAgreementOperationOutput (PublisherOperationOutputF a) (PublisherOperationOutputF a') =
        PublisherOperationOutputF (a <> a')

    functorizeAgreementOperationOutput = fmap MkAnySemigroupMonetaryUnitData

    data AgreementOperation (PublisherContract sft) = UpdateDistributionFlowRate (SFT_MVAL sft)

    type AgreementOperationOutput (PublisherContract sft) = PublisherOperationOutputF sft

    data AgreementOperationOutputF (PublisherContract sft) elem = PublisherOperationOutputF
        elem -- publisher mud
        deriving stock (Functor, Foldable, Traversable, Generic)

type PublisherOperationOutputF sft = AgreementOperationOutputF (PublisherContract sft)
    (PublisherMonetaryUnitData sft)

instance SuperfluidSystemTypes sft => Default (PublisherOperationOutputF sft)

-- * Subscriber Operations

instance SuperfluidSystemTypes sft => MonetaryUnitDataClass (SubscriberContract sft) sft where
    balanceProvided = balanceProvided . CFMUD.MkMonetaryUnitData

instance SuperfluidSystemTypes sft => AgreementContract (SubscriberContract sft) sft where
    applyAgreementOperation ((dcBase, dc), (scBase, sc)) SettleSubscription t' = let
        settledΔ = dcfr * fromIntegral (t' - t_dc)
        vpuΔ'    = if tu /= 0 then floor $ fromIntegral settledΔ / tu else 0

        vpu' = vpu_i + vpuΔ'
        dc'  = dc { dc_updated_at = t'
                  , dc_value_per_unit = vpu'
                  }

        svΔ = floor $ fromIntegral (vpu' - svpu) * u
        sc' = sc { sc_settled_value = UntappedValue $ sv + svΔ
                 , sc_settled_value_per_unit = vpu'
                 }

        muds = def & set CFMUD.settledAt t'

        in ( ((dcBase, dc'), (scBase, sc'))
           , CFMUD.MkMonetaryUnitData <$> SubscriberOperationOutputF muds)

        where DistributionContractBase { total_unit            = tu
                                       } = dcBase
              DistributionContract { dc_updated_at             = t_dc
                                   , dc_value_per_unit         = vpu_i
                                   , dc_flow_rate              = dcfr
                                   } = dc
              SubscriptionContractBase { sub_owned_unit        = u
                                   } = scBase
              SubscriptionContract { sc_settled_value          = UntappedValue sv
                                   , sc_settled_value_per_unit = svpu
                                   } = sc

    concatAgreementOperationOutput (SubscriberOperationOutputF a) (SubscriberOperationOutputF a') =
        SubscriberOperationOutputF (a <> a')

    functorizeAgreementOperationOutput = fmap MkAnySemigroupMonetaryUnitData

    data AgreementOperation (SubscriberContract sft) = SettleSubscription

    type AgreementOperationOutput (SubscriberContract sft) = SubscriberOperationOutputF sft

    data AgreementOperationOutputF (SubscriberContract sft) elem = SubscriberOperationOutputF
        elem -- publisher mud
        deriving stock (Functor, Foldable, Traversable, Generic)

type SubscriberOperationOutputF sft = AgreementOperationOutputF (SubscriberContract sft)
    (PublisherMonetaryUnitData sft)

instance SuperfluidSystemTypes sft => Default (SubscriberOperationOutputF sft)
