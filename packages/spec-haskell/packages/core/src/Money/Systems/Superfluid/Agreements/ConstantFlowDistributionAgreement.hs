{-# OPTIONS_GHC -Wno-orphans #-}
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
import           Data.Kind                                                                 (Type)

import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import           Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow         as CFMUD

-- * Monetary unit data

type PublisherMonetaryUnitData sft = CFMUD.MonetaryUnitData (PublisherData sft) sft

type SubscriberMonetaryUnitData sft = CFMUD.MonetaryUnitData (SubscriberData sft) sft

instance SuperfluidTypes sft => CFMUD.MonetaryUnitLenses (PublisherData sft) sft where
    settledAt = $(field 'cfda_pub_settled_at)
    settledUntappedValue = $(field 'cfda_pub_settled_untapped_value)
    netFlowRate = $(field 'cfda_pub_total_flow_rate)
    settledBufferValue = lens (const 0) const

instance SuperfluidTypes sft => CFMUD.MonetaryUnitLenses (SubscriberData sft) sft where
    settledAt     = readOnlyLens
        (\(SubscriberData _ (SubscriptionContract { sub_settled_at = t })) -> t)
    netFlowRate   = readOnlyLens
        (\(SubscriberData
            (DistributionContract { total_unit     = tu
                                  , cfda_flow_rate = dcfr })
            (SubscriptionContract { sub_owned_unit = u })
          ) -> floor $ fromIntegral dcfr * u / tu )
    settledUntappedValue = readOnlyLens
        (\(SubscriberData
           (DistributionContract { total_unit                      = tu
                                 , cfda_value_per_unit             = vpu
                                 , cfda_flow_rate                  = dcfr
                                 , cfda_settled_at                 = t_dc
                                 })
           (SubscriptionContract { sub_owned_unit                  = u
                                 , sub_settled_at                  = t_sc
                                 , cfda_sub_settled_value          = UntappedValue sv
                                 , cfda_sub_settled_value_per_unit = svpu })
          ) -> let vpuΔ = floor $ fromIntegral dcfr * fromIntegral (t_dc - t_sc) / tu
               in  UntappedValue $ sv + floor (u * fromIntegral (vpu - svpu - vpuΔ)))
    settledBufferValue = readOnlyLens (const 0)

-- * Publisher Operations

newtype PublisherOperation sft = UpdateDistributionFlowRate (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (PublisherOperation sft) sft where
    data AgreementOperationData (PublisherOperation sft) = PublisherOperationData (DistributionContract sft)
    data AgreementOperationResultF (PublisherOperation sft) elem = PublisherOperationResultF elem -- publisher amud
        deriving stock (Functor, Foldable, Traversable)
    type AgreementMonetaryUnitDataInOperation (PublisherOperation sft) = PublisherMonetaryUnitData sft

    applyAgreementOperation (UpdateDistributionFlowRate dcfr') (PublisherOperationData dc) t' = let
        pub'  = dc { cfda_settled_at = t'
                   , cfda_value_per_unit = vpu + vpuΔ
                   , cfda_flow_rate = dcfr'
                   }
        aorΔ  = PublisherOperationResultF
                  (def & set CFMUD.settledAt t'
                       & set CFMUD.netFlowRate (dcfr - dcfr') -- reverse sign for outgoing flow
                       & set CFMUD.settledUntappedValue (UntappedValue (-settledΔ)))
        in (PublisherOperationData pub', fmap CFMUD.MkMonetaryUnitData aorΔ)
        where DistributionContract { total_unit          = tu
                                   , cfda_settled_at     = t_dc
                                   , cfda_value_per_unit = vpu
                                   , cfda_flow_rate      = dcfr
                                   } = dc
              -- FIXME can be a black hole due to tu == 0 or precision
              settledΔ = dcfr * fromIntegral (t' - t_dc)
              vpuΔ = if tu /= 0 then floor $ fromIntegral settledΔ / tu else 0

type PublisherOperationData :: Type -> Type
type PublisherOperationData sft = AgreementOperationData (PublisherOperation sft)

-- * Subscriber Operations

data SubscriberOperation sft = SettleSubscription

instance SuperfluidTypes sft => AgreementOperation (SubscriberOperation sft) sft where
    data AgreementOperationData (SubscriberOperation sft) = SubscriberOperationData (SubscriberData sft)
    data AgreementOperationResultF (SubscriberOperation sft) elem = SubscriberOperationPartiesF elem
        deriving stock (Functor, Foldable, Traversable)
    type AgreementMonetaryUnitDataInOperation (SubscriberOperation sft) = PublisherMonetaryUnitData sft

    applyAgreementOperation SettleSubscription (SubscriberOperationData sub) t' = let
        sub'  = SubscriberData
                  (dc { cfda_settled_at = t'
                      , cfda_value_per_unit = vpu'
                      })
                  (sc { cfda_sub_settled_value = UntappedValue $ sv + svΔ
                      , cfda_sub_settled_value_per_unit = vpu'
                      })
        aorΔ  = SubscriberOperationPartiesF
                  (def & set CFMUD.settledAt t'
                       & set CFMUD.settledUntappedValue (UntappedValue (-settledΔ)))
        in (SubscriberOperationData sub', fmap CFMUD.MkMonetaryUnitData aorΔ)
        where (SubscriberData
                 dc@(DistributionContract { total_unit                      = tu
                                          , cfda_settled_at                 = t_dc
                                          , cfda_value_per_unit             = vpu_i
                                          , cfda_flow_rate                  = dcfr
                                          })
                 sc@(SubscriptionContract { sub_owned_unit                  = u
                                          , cfda_sub_settled_value          = UntappedValue sv
                                          , cfda_sub_settled_value_per_unit = svpu
                                          })) = sub
              settledΔ = dcfr * fromIntegral (t' - t_dc)
              vpuΔ' = if tu /= 0 then floor $ fromIntegral settledΔ / tu else 0
              vpu' = vpu_i + vpuΔ'
              svΔ = floor $ fromIntegral (vpu' - svpu) * u

type SubscriberOperationData :: Type -> Type
type SubscriberOperationData sft = AgreementOperationData (SubscriberOperation sft)
