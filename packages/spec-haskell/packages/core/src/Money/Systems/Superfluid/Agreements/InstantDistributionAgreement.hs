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

instance SuperfluidTypes sft => AgreementOperation (IDAPublisherOperation sft)
    (DistributionContract sft) (IDAPublisherMonetaryUnitData sft) sft where
    data AgreementOperationResultF (IDAPublisherOperation sft) elem = IDAOPublisherOperationPartiesF elem
        deriving stock (Functor, Foldable, Traversable)

    applyAgreementOperation (Distribute amount) aod _ = let
        aod'  = aod { value_per_unit = floor (fromIntegral p + delta) }
        aorΔ = fmap ITMUD.MkMonetaryUnitData (IDAOPublisherOperationPartiesF
                    (def & set ITMUD.untappedValue (coerce (- amount))))
        in (aod', aorΔ)
        where DistributionContract { total_unit = tu, value_per_unit = p } = aod
              delta = fromIntegral amount / tu

-- * Subscriber Operations

data IDASubscriberOperation sft = Subscribe   (SFT_FLOAT sft) |
                                  Unsubscribe

instance SuperfluidTypes sft => AgreementOperation (IDASubscriberOperation sft)
    (SubscriberData sft) (IDASubscriberMonetaryUnitData sft) sft where
    data AgreementOperationResultF (IDASubscriberOperation sft) elem = IDASubscriberOperationPartiesF

    applyAgreementOperation (Subscribe unit) aod _ = let
        aod'  = SubscriberData
                  (dc { total_unit = tu + unit })
                  (sc { owned_unit = u + unit
                      , settled_value_per_unit = vpu
                      , settled_value = UntappedValue sv'
                      })
        aorΔ = IDASubscriberOperationPartiesF
        in (aod', aorΔ)
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
        -- FIXME operation missing
        in (aod, aorΔ)
        where aorΔ = IDASubscriberOperationPartiesF
