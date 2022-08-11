{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex where

import           Data.Default
import           Data.Kind                                                             (Type)
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowDistributionAgreement as CFDA
import qualified Money.Systems.Superfluid.Agreements.InstantDistributionAgreement      as IDA
import           Money.Systems.Superfluid.Agreements.ProportionalDistributionCommon


-- * Contracts

-- | Agreement contract for a distribution. Its sole party is also known as the "publisher".
data DistributionContract sft = DistributionContract
    { dc_base :: DistributionContractBase sft
    , dc_ida  :: IDA.DistributionContract sft
    , dc_cfda :: CFDA.DistributionContract sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (DistributionContract sft)

-- | Agreement contract for a subscription to a distribution. Its sole party is also known as the "subscriber".
data SubscriptionContract sft = SubscriptionContract
    { sc_base :: SubscriptionContractBase sft
    , sc_ida  :: IDA.SubscriptionContract sft
    , sc_cfda :: CFDA.SubscriptionContract sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (SubscriptionContract sft)

-- * Monetary unit data

-- | Distribution publisher data.
--
-- Note: This is going to be very similar to the universal data. The separation is perhaps mostly for data tracking
--       reason. TODO: maybe really just use universal data for publisher side too?
data PublisherData sft = PublisherData
    { pub_ida  :: IDA.PublisherData sft
    , pub_cfda :: CFDA.PublisherData sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (PublisherData sft)

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
    -> (DistributionContractBase sft, IDA.DistributionContract sft,
        SubscriptionContractBase sft, IDA.SubscriptionContract sft)
ida_sub_data a = ((dc_base.fst) a, (dc_ida.fst) a, (sc_base.snd) a, (sc_ida.snd) a)

cfda_sub_data
    :: SubscriberData sft
    -> (DistributionContractBase sft, CFDA.DistributionContract sft,
        SubscriptionContractBase sft, CFDA.SubscriptionContract sft)
cfda_sub_data a = ((dc_base.fst) a, (dc_cfda.fst) a, (sc_base.snd) a, (sc_cfda.snd) a)

-- * Subscriber Operations

data SubscriptionOperation sft = Subscribe (SFT_FLOAT sft)

instance SuperfluidTypes sft => AgreementOperation (SubscriptionOperation sft) sft where
    applyAgreementOperation (Subscribe unit) (SubscriberOperationData sub) t' = let
        sub'  = ( dc { dc_base = DistributionContractBase { total_unit = tu + unit }}
                , sc { sc_base = SubscriptionContractBase
                         { sub_owned_unit = u + unit
                         , sub_settled_at = t'
                         }})
        in (SubscriberOperationData sub', SubscriptionOperationPartiesF)
        where ( dc@(DistributionContract { dc_base = DistributionContractBase { total_unit = tu }})
                  , sc@(SubscriptionContract { sc_base = SubscriptionContractBase { sub_owned_unit = u }}))
                  = sub

    data AgreementContract (SubscriptionOperation sft) = SubscriberOperationData (SubscriberData sft)
    data AgreementOperationResultF (SubscriptionOperation sft) elem = SubscriptionOperationPartiesF
    type MonetaryUnitDataInOperation (SubscriptionOperation sft) = NullMonetaryUnitData sft

type SubscriberOperationData :: Type -> Type
type SubscriberOperationData sft = AgreementContract (SubscriptionOperation sft)

-- TODO merge to SubscriptionOperation
updateSubscription :: SuperfluidTypes sft
    => SubscriberData sft
    -> SFT_FLOAT sft
    -> SFT_TS sft
    -> ( SubscriberData sft
       , CFDA.PublisherMonetaryUnitData sft)
updateSubscription (dc0, sc0) unit t = let
    (dc1, sc1) = settle_ida (dc0, sc0)
    (dc2, sc2, cfdaMUDΔ) = settle_cfda (dc1, sc1)
    (dc', sc') = subscribe_unit (dc2, sc2)
    in ((dc', sc'), cfdaMUDΔ)
    where
        settle_ida (dc, sc) = let
            acd = (dc_base dc, dc_ida dc, sc_base sc, sc_ida sc)
            acd' = applyAgreementOperation IDA.SettleSubscription (IDA.SubscriberContract acd) t
            (IDA.SubscriberContract (_, dc_ida', _, sc_ida'), _) = acd'
            in (dc { dc_ida = dc_ida' }, sc { sc_ida = sc_ida' })
        settle_cfda (dc, sc) = let
            acd = (dc_base dc, dc_cfda dc, sc_base sc, sc_cfda sc)
            acd' = applyAgreementOperation CFDA.SettleSubscription (CFDA.SubscriberContract acd) t
            ( CFDA.SubscriberContract (_, dc_cfda', _, sc_cfda')
                , CFDA.SubscriberOperationPartiesF cfdaMUDΔ ) = acd'
            in (dc { dc_cfda = dc_cfda' }, sc { sc_cfda = sc_cfda' }, cfdaMUDΔ)
        subscribe_unit (dc, sc) = let
            acd = (dc, sc)
            acd' = applyAgreementOperation (Subscribe unit) (SubscriberOperationData acd) t
            (SubscriberOperationData (dc', sc'), _) = acd'
            in (dc', sc')
