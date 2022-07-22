{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | IDA is IDA over the Proportional Distribution Index
--
module Money.Systems.Superfluid.Agreements.InstantDistributionAgreement where

import           Data.Coerce
import           Data.Default

import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantTransfer      as ITMUD

import           Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex

-- | * Monetary unit data

type IDAPublisherMonetaryUnitData sft = ITMUD.MonetaryUnitData (PublisherData sft) sft

type IDASubscriberMonetaryUnitData sft = ITMUD.MonetaryUnitData (SubscriberData sft) sft

instance SuperfluidTypes sft => ITMUD.MonetaryUnitLenses (PublisherData sft) sft where
    untappedValue = $(field 'distributed_value)

-- | The contract is the data. It is not scalable.
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

-- | * Operations

data IDAPublisherOperation sft = Distribute (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (IDAPublisherOperation sft)
    (DistributionContract sft) (IDAPublisherMonetaryUnitData sft) sft where
    data AgreementOperationPartiesF (IDAPublisherOperation sft) elem = IDAOPublisherOperationPartiesF elem
        deriving stock (Functor, Foldable, Traversable)

    applyAgreementOperation (Distribute amount) aod _ = let
        aod'  = aod { value_per_unit = floor (fromIntegral p + delta) }
        aopsΔ = fmap ITMUD.MkMonetaryUnitData (IDAOPublisherOperationPartiesF
                    (def & set ITMUD.untappedValue (coerce (- amount))))
        in (aod', aopsΔ)
        where DistributionContract { total_unit = tu, value_per_unit = p } = aod
              delta = fromIntegral amount / tu

data IDASubscriberOperation sft = Subscribe   (SFT_FLOAT sft) |
                                  Unsubscribe

instance SuperfluidTypes sft => AgreementOperation (IDASubscriberOperation sft)
    (SubscriberData sft) (IDASubscriberMonetaryUnitData sft) sft where
    data AgreementOperationPartiesF (IDASubscriberOperation sft) elem = IDASubscriberOperationPartiesF elem
        deriving stock (Functor, Foldable, Traversable)

    applyAgreementOperation (Subscribe unit) aod _ = let
        aod'  = SubscriberData
                  (dc { total_unit = tu + unit })
                  (sc { owned_unit = u + unit
                      , settled_value_per_unit = vpu
                      , settled_value = UntappedValue sv'
                      })
        aopsΔ = fmap ITMUD.MkMonetaryUnitData (IDASubscriberOperationPartiesF def)
        in (aod', aopsΔ)
        where (SubscriberData
                dc@(DistributionContract
                    { total_unit = tu
                    , value_per_unit = vpu })
                sc@(SubscriptionContract
                    { owned_unit = u
                    , settled_value_per_unit = svpu
                    , settled_value = UntappedValue sv
                    })) = aod
              sv' = floor (fromIntegral sv + fromIntegral (vpu - svpu) * u)
    applyAgreementOperation Unsubscribe aod _ = let
        in (aod, aopsΔ)
        where aopsΔ = fmap ITMUD.MkMonetaryUnitData (IDASubscriberOperationPartiesF def)
