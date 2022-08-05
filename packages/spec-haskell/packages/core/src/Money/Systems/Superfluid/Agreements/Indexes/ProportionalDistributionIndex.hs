{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex where

import           Data.Default
import           Data.Kind                         (Type)
import           GHC.Generics


import           Money.Systems.Superfluid.Concepts

-- * Contracts

-- | Agreement contract for a distribution. Its sole party is also known as the "publisher".
data DistributionContract sft = DistributionContract
    { total_unit          :: SFT_FLOAT sft
    -- IDA
    , ida_value_per_unit  :: SFT_MVAL sft
    -- CFDA
    , cfda_settled_at     :: SFT_TS sft
    , cfda_value_per_unit :: SFT_MVAL sft
    , cfda_flow_rate      :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (DistributionContract sft)

-- | Agreement contract for a subscription to a distribution. Its sole party is also known as the "subscriber".
data SubscriptionContract sft = SubscriptionContract
    { sub_owned_unit                  :: SFT_FLOAT sft
    , sub_settled_at                  :: SFT_TS sft
    -- IDA
    , ida_sub_settled_value           :: UntappedValue (SFT_MVAL sft)
    , ida_sub_settled_value_per_unit  :: SFT_MVAL sft
    -- CFDA
    , cfda_sub_settled_value          :: UntappedValue (SFT_MVAL sft)
    , cfda_sub_settled_value_per_unit :: SFT_MVAL sft
    } deriving (Generic)

deriving instance SuperfluidTypes sft => Default (SubscriptionContract sft)

-- * Monetary unit data

-- | Distribution publisher data.
--
-- Note: This is going to be very similar to the universal data. The separation is perhaps mostly for data tracking
--       reason. TODO: maybe really just use universal data for publisher side too?
data PublisherData sft = PublisherData
    -- IDA
    { distributed_value               :: UntappedValue (SFT_MVAL sft)
    -- CFDA
    , cfda_pub_settled_at             :: SFT_TS sft
    , cfda_pub_settled_untapped_value :: UntappedValue (SFT_MVAL sft)
    , cfda_pub_total_flow_rate        :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (PublisherData sft)

-- | Distribution subcriber data.
--
-- Note: The contracts are the subscriber data. It is not storage-scalable.
data SubscriberData sft = SubscriberData
    { distribution_contract :: DistributionContract sft
    , subscription_contract :: SubscriptionContract sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (SubscriberData sft)

-- * Subscriber Operations

data CommonSubscriberOperation sft = Subscribe (SFT_FLOAT sft)

instance SuperfluidTypes sft => AgreementOperation (CommonSubscriberOperation sft) sft where
    data AgreementOperationData (CommonSubscriberOperation sft) = CommonSubscriberOperationData (SubscriberData sft)
    data AgreementOperationResultF (CommonSubscriberOperation sft) elem = CommonSubscriberOperationPartiesF
    type AgreementMonetaryUnitDataInOperation (CommonSubscriberOperation sft) = NullAgreementMonetaryUnitData sft

    applyAgreementOperation (Subscribe unit) (CommonSubscriberOperationData sub) t' = let
        sub'  = SubscriberData
                  (dc { total_unit = tu + unit })
                  (sc { sub_owned_unit = u + unit
                      , sub_settled_at = t'
                      })
        in (CommonSubscriberOperationData sub', CommonSubscriberOperationPartiesF)
        where (SubscriberData
                 dc@(DistributionContract { total_unit = tu })
                 sc@(SubscriptionContract { sub_owned_unit = u })) = sub

type SubscriberOperationData :: Type -> Type
type SubscriberOperationData sft = AgreementOperationData (CommonSubscriberOperation sft)
