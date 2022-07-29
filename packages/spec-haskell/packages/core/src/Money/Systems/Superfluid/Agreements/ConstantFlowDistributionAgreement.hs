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

type CFDAPublisherMonetaryUnitData sft = CFMUD.MonetaryUnitData (PublisherData sft) sft

type CFDASubscriberMonetaryUnitData sft = CFMUD.MonetaryUnitData (SubscriberData sft) sft

instance SuperfluidTypes sft => CFMUD.MonetaryUnitLenses (PublisherData sft) sft where
    settledAt = $(field 'cfda_pub_settled_at)
    settledUntappedValue = $(field 'cfda_pub_settled_untapped_value)
    netFlowRate = $(field 'cfda_total_flow_rate)
    settledBufferValue = lens (const 0) const -- TODO remove it

instance SuperfluidTypes sft => CFMUD.MonetaryUnitLenses (SubscriberData sft) sft where
    settledAt     = readOnlyLens
        -- lens getter: subscribed value
        (\(SubscriberData
            _
            (SubscriptionContract { cfda_sub_settled_at = t
                                  })
          ) -> t)
    netFlowRate   = readOnlyLens
        -- lens getter: subscribed value
        (\(SubscriberData
            (DistributionContract { cfda_flow_rate_per_unit = frpu })
            (SubscriptionContract { owned_unit = u })
          ) -> floor $ fromIntegral frpu * u)
    settledUntappedValue = readOnlyLens
        -- lens getter: subscribed value
        (\(SubscriberData
            _
            (SubscriptionContract { cfda_sub_settled_value = sv
                                  })
          ) -> sv)
    settledBufferValue = readOnlyLens (const 0)

-- * Publisher Operations

newtype CFDAPublisherOperation sft = UpdateDistributionFlowRate (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (CFDAPublisherOperation sft) sft where
    data AgreementOperationData (CFDAPublisherOperation sft) = PublisherOperationData (DistributionContract sft)
    data AgreementOperationResultF (CFDAPublisherOperation sft) elem = CFDAPublisherOperationResultF elem -- publisher amud
        deriving stock (Functor, Foldable, Traversable)
    type AgreementMonetaryUnitDataInOperation (CFDAPublisherOperation sft) = CFDAPublisherMonetaryUnitData sft

    applyAgreementOperation (UpdateDistributionFlowRate fr) (PublisherOperationData pub) t' = let
        pub'  = pub { cfda_settled_at = t', cfda_flow_rate_per_unit = frpu' }
        aorΔ  = CFDAPublisherOperationResultF
                  (def & set CFMUD.settledAt t'
                       & set CFMUD.netFlowRate frΔ
                       & set CFMUD.settledUntappedValue (UntappedValue settledΔ))
        in (PublisherOperationData pub', fmap CFMUD.MkMonetaryUnitData aorΔ)
        where DistributionContract { cfda_settled_at = t
                                   , total_unit = tu
                                   , cfda_flow_rate_per_unit = frpu } = pub
              frΔ   = floor $ fromIntegral fr - fromIntegral frpu * tu
              frpu' = floor $ fromIntegral fr / tu
              settledΔ = frpu * fromIntegral (t' - t)

type PublisherOperationData :: Type -> Type
type PublisherOperationData sft = AgreementOperationData (CFDAPublisherOperation sft)
