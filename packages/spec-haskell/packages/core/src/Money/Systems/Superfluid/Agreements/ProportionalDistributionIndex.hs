{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex where

import           Data.Default
import           Data.Type.Any
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes
--
import           Money.Systems.Superfluid.Agreements.ProportionalDistribution.Common
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistribution.ConstantFlowDistributionAgreement as CFDA
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistribution.InstantDistributionAgreement      as IDA


-- * Contracts

-- | Agreement contract for a distribution. Its sole party is also known as the "publisher".
data DistributionContract sft = DistributionContract
    { dc_base :: DistributionContractBase sft
    , dc_ida  :: IDA.DistributionContract sft
    , dc_cfda :: CFDA.DistributionContract sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (DistributionContract sft)

-- | Agreement contract for a subscription to a distribution. Its sole party is also known as the "subscriber".
data SubscriptionContract sft = SubscriptionContract
    { sc_base :: SubscriptionContractBase sft
    , sc_ida  :: IDA.SubscriptionContract sft
    , sc_cfda :: CFDA.SubscriptionContract sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (SubscriptionContract sft)

-- * Monetary unit data

-- | Distribution publisher data.
data PublisherData sft = PublisherData
    { pub_ida  :: IDA.PublisherData sft
    , pub_cfda :: CFDA.PublisherData sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (PublisherData sft)

pub_ida_lenses :: Lens' (PublisherData sft) (IDA.PublisherData sft)
pub_ida_lenses = $(field 'pub_ida)
pub_cfda_lenses :: Lens' (PublisherData sft) (CFDA.PublisherData sft)
pub_cfda_lenses = $(field 'pub_cfda)

-- | Distribution subcriber data.
--
-- Note: The contracts are the subscriber data. It is not storage-scalable.
type SubscriberData sft = (DistributionContract sft, SubscriptionContract sft)

ida_sub_data
    :: SubscriberData sft
    -> (IDA.DistributionContractFull sft, IDA.SubscriptionContractFull sft)
ida_sub_data a = (((dc_base.fst) a, (dc_ida.fst) a), ((sc_base.snd) a, (sc_ida.snd) a))

cfda_sub_data
    :: SubscriberData sft
    -> (CFDA.DistributionContractFull sft, CFDA.SubscriptionContractFull sft)
cfda_sub_data a = (((dc_base.fst) a, (dc_cfda.fst) a), ((sc_base.snd) a, (sc_cfda.snd) a))

-- * Subscriber Operations

type SubscriberContract sft = (DistributionContract sft , SubscriptionContract sft)

settle_ida :: SuperfluidSystemTypes sft => SubscriberContract sft -> SFT_TS sft
           -> SubscriberContract sft
settle_ida (dc, sc) t = let
    ac = ((dc_base dc, dc_ida dc), (sc_base sc, sc_ida sc))
    ac' = applyAgreementOperation ac IDA.SettleSubscription t
    (((_, dc_ida'), (_, sc_ida')), _) = ac'
    in (dc { dc_ida = dc_ida' }, sc { sc_ida = sc_ida' })

settle_cfda :: SuperfluidSystemTypes sft => SubscriberContract sft -> SFT_TS sft
            -> (SubscriberContract sft, CFDA.PublisherMonetaryUnitData sft)
settle_cfda (dc, sc) t = let
    ac = ((dc_base dc, dc_cfda dc), (sc_base sc, sc_cfda sc))
    ac' = applyAgreementOperation ac CFDA.SettleSubscription t
    ( ((_, dc_cfda'), (_, sc_cfda'))
        , CFDA.SubscriberOperationOutputF cfdaMUDΔ ) = ac'
    in ((dc { dc_cfda = dc_cfda' }, sc { sc_cfda = sc_cfda' }), cfdaMUDΔ)

instance SuperfluidSystemTypes sft => MonetaryUnitDataClass (SubscriberContract sft) sft where
    balanceProvided ac t = balanceProvided (ida_sub_data ac) t <>
                           balanceProvided (cfda_sub_data ac) t

instance SuperfluidSystemTypes sft => AgreementContract (SubscriberContract sft) sft where
    applyAgreementOperation (dc0, sc0) (Subscribe unit) t' = let
        (dc1, sc1) = settle_ida (dc0, sc0) t'
        ((dc2, sc2), cfdaMUDΔ) = settle_cfda (dc1, sc1) t'
        ac' = ( dc2 { dc_base = DistributionContractBase
                                { total_unit = tu + unit }}
              , sc2 { sc_base = SubscriptionContractBase
                                { sub_owned_unit = u + unit
                                , sub_settled_at = t'
                                }})

        in (ac', SubscriberOperationOutput cfdaMUDΔ)

        where DistributionContract { dc_base = DistributionContractBase { total_unit = tu }} = dc0
              SubscriptionContract { sc_base = SubscriptionContractBase { sub_owned_unit = u }} = sc0

    functorizeAgreementOperationOutput p (SubscriberOperationOutput cfda) =
        SubscriberOperationOutputF (mkAny p cfda)

    data AgreementOperation (SubscriberContract sft) = Subscribe (SFT_FLOAT sft)

    type AgreementOperationOutput (SubscriberContract sft) = SubscriberOperationOutput sft

    data AgreementOperationOutputF (SubscriberContract sft) elem = SubscriberOperationOutputF
        { subscription_output_cfda :: elem
        } deriving stock (Functor, Foldable, Traversable)

data SubscriberOperationOutput sft = SubscriberOperationOutput
    (CFDA.PublisherMonetaryUnitData sft)
    deriving stock (Generic)

instance SuperfluidSystemTypes sft => Default (SubscriberOperationOutput sft)
instance SuperfluidSystemTypes sft => Monoid (SubscriberOperationOutput sft) where mempty = def
instance SuperfluidSystemTypes sft => Semigroup (SubscriberOperationOutput sft) where (<>) = const
