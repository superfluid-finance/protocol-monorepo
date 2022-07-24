{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Instant distribution agreement.
--
-- It is instant transferring over an proportional distribution index
--
-- This module is typically imported using qualified name IDA.
module Money.Systems.Superfluid.Agreements.InstantDistributionAgreement where

import           Data.Coerce
import           Data.Default
import           Data.Kind                                                                 (Type)

import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import           Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantTransfer      as ITMUD

-- * Monetary unit data

type IDAPublisherMonetaryUnitData sft = ITMUD.MonetaryUnitData (PublisherData sft) sft

type IDASubscriberMonetaryUnitData sft = ITMUD.MonetaryUnitData (SubscriberData sft) sft

instance SuperfluidTypes sft => ITMUD.MonetaryUnitLenses (PublisherData sft) sft where
    untappedValue = $(field 'distributed_value)

instance SuperfluidTypes sft => ITMUD.MonetaryUnitLenses (SubscriberData sft) sft where
    untappedValue = readOnlyLens
        -- lens getter: subscribed value
        (\(SubscriberData
            (DistributionContract { value_per_unit = vpu })
            (SubscriptionContract
             { settled_value_per_unit = svpu
             , settled_value = sv
             , owned_unit = u
             })
          ) -> (+) sv $ UntappedValue $ floor $
            u * fromIntegral (vpu - svpu))

-- * Publisher Operations

data IDAPublisherOperation sft = Distribute (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (IDAPublisherOperation sft) (IDAPublisherMonetaryUnitData sft) sft where
    data AgreementOperationData (IDAPublisherOperation sft) = PublisherOperationData (DistributionContract sft)
    data AgreementOperationResultF (IDAPublisherOperation sft) elem = IDAPublisherOperationResultF elem
        deriving stock (Functor, Foldable, Traversable)

    applyAgreementOperation (Distribute amount) (PublisherOperationData pub) _ = let
        pub'  = pub { value_per_unit = floor (fromIntegral p + delta) }
        aorΔ = fmap ITMUD.MkMonetaryUnitData (IDAPublisherOperationResultF
                    (def & set ITMUD.untappedValue (coerce (- amount))))
        in (PublisherOperationData pub', aorΔ)
        where DistributionContract { total_unit = tu, value_per_unit = p } = pub
              delta = fromIntegral amount / tu

type PublisherOperationData :: Type -> Type
type PublisherOperationData sft = AgreementOperationData (IDAPublisherOperation sft)

-- * Subscriber Operations

data IDASubscriberOperation sft = Subscribe   (SFT_FLOAT sft) |
                                  Unsubscribe

instance SuperfluidTypes sft => AgreementOperation (IDASubscriberOperation sft) (IDASubscriberMonetaryUnitData sft) sft where
    data AgreementOperationData (IDASubscriberOperation sft) = SubscriberOperationData (SubscriberData sft)
    data AgreementOperationResultF (IDASubscriberOperation sft) elem = IDASubscriberOperationPartiesF

    applyAgreementOperation (Subscribe unit) (SubscriberOperationData sub) _ = let
        sub'  = SubscriberData
                  (dc { total_unit = tu + unit })
                  (sc { owned_unit = u + unit
                      , settled_value_per_unit = vpu
                      , settled_value = UntappedValue sv'
                      })
        in (SubscriberOperationData sub', IDASubscriberOperationPartiesF)
        where (SubscriberData
                dc@(DistributionContract
                    { total_unit = tu
                    , value_per_unit = vpu })
                sc@(SubscriptionContract
                    { owned_unit = u
                    , settled_value_per_unit = svpu
                    , settled_value = UntappedValue sv
                    })) = sub
              sv' = floor (fromIntegral sv + fromIntegral (vpu - svpu) * u)
    applyAgreementOperation Unsubscribe (SubscriberOperationData sub) _ = let
        -- FIXME operation missing
        sub' = sub
        in (SubscriberOperationData sub', IDASubscriberOperationPartiesF)

type SubscriberOperationData :: Type -> Type
type SubscriberOperationData sft = AgreementOperationData (IDASubscriberOperation sft)
