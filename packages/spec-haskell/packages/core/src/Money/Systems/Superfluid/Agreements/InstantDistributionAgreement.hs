{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Instant distribution agreement.
--
-- It is instant transferring over an proportional distribution index
--
-- This module is typically imported using qualified name .
module Money.Systems.Superfluid.Agreements.InstantDistributionAgreement where

import           Data.Coerce
import           Data.Default
import           Data.Kind                                                                 (Type)

import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import           Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantValue         as IVMUD

-- * Monetary unit data

type PublisherMonetaryUnitData sft = IVMUD.MonetaryUnitData (PublisherData sft) sft

type SubscriberMonetaryUnitData sft = IVMUD.MonetaryUnitData (SubscriberData sft) sft

instance SuperfluidTypes sft => IVMUD.MonetaryUnitLenses (PublisherData sft) sft where
    untappedValue = $(field 'distributed_value)

instance SuperfluidTypes sft => IVMUD.MonetaryUnitLenses (SubscriberData sft) sft where
    untappedValue = readOnlyLens
        -- lens getter: subscribed value
        (\(SubscriberData
            (DistributionContract { ida_value_per_unit = vpu })
            (SubscriptionContract { ida_sub_settled_value = UntappedValue sv
                                  , ida_sub_settled_value_per_unit = svpu
                                  , sub_owned_unit = u
                                  })
          ) -> UntappedValue $ sv + floor (u * fromIntegral (vpu - svpu)))

-- * Publisher Operations

newtype PublisherOperation sft = Distribute (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (PublisherOperation sft) sft where
    data AgreementOperationData (PublisherOperation sft) = PublisherOperationData (DistributionContract sft)
    data AgreementOperationResultF (PublisherOperation sft) elem = PublisherOperationResultF elem -- publisher amud
        deriving stock (Functor, Foldable, Traversable)
    type AgreementMonetaryUnitDataInOperation (PublisherOperation sft) = PublisherMonetaryUnitData sft

    applyAgreementOperation (Distribute amount) (PublisherOperationData pub) _ = let
        pub'  = pub { ida_value_per_unit = floor (fromIntegral vpu + vpuΔ) }
        aorΔ  = PublisherOperationResultF
                  (def & set IVMUD.untappedValue (coerce (- amount)))
        in (PublisherOperationData pub', fmap IVMUD.MkMonetaryUnitData aorΔ)
        where DistributionContract { total_unit = tu, ida_value_per_unit = vpu } = pub
              vpuΔ = fromIntegral amount / tu

type PublisherOperationData :: Type -> Type
type PublisherOperationData sft = AgreementOperationData (PublisherOperation sft)

-- * Subscriber Operations

data SubscriberOperation sft = SettleSubscription

instance SuperfluidTypes sft => AgreementOperation (SubscriberOperation sft) sft where
    data AgreementOperationData (SubscriberOperation sft) = SubscriberOperationData (SubscriberData sft)
    data AgreementOperationResultF (SubscriberOperation sft) elem = SubscriberOperationPartiesF
    type AgreementMonetaryUnitDataInOperation (SubscriberOperation sft) = NullAgreementMonetaryUnitData sft

    applyAgreementOperation SettleSubscription (SubscriberOperationData sub) _ = let
        sub'  = SubscriberData
                  dc
                  (sc { ida_sub_settled_value = UntappedValue $ sv + svΔ
                      , ida_sub_settled_value_per_unit = vpu
                      })
        in (SubscriberOperationData sub', SubscriberOperationPartiesF)
        where (SubscriberData
                 dc@(DistributionContract { ida_value_per_unit = vpu })
                 sc@(SubscriptionContract { sub_owned_unit = u
                                          , ida_sub_settled_value = UntappedValue sv
                                          , ida_sub_settled_value_per_unit = svpu
                                          })) = sub
              svΔ = floor $ fromIntegral (vpu - svpu) * u

type SubscriberOperationData :: Type -> Type
type SubscriberOperationData sft = AgreementOperationData (SubscriberOperation sft)
