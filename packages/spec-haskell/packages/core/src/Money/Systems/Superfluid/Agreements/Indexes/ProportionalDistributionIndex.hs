{-# LANGUAGE DeriveAnyClass #-}

module Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex where

import           Data.Default
import           GHC.Generics


import           Money.Systems.Superfluid.Concepts

-- * Contracts

-- | Agreement contract for a distribution. Its sole party is also known as the "publisher".
data DistributionContract sft = DistributionContract
    { total_unit         :: SFT_FLOAT sft
    -- IDA
    , value_per_unit     :: SFT_MVAL sft
    -- FDA
    , flow_rate_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (DistributionContract sft)

-- | Agreement contract for a subscription to a distribution. Its sole party is also known as the "subscriber".
data SubscriptionContract sft = SubscriptionContract
    { owned_unit                 :: SFT_FLOAT sft
    , settled_value              :: UntappedValue (SFT_MVAL sft)
    -- IDA
    , settled_value_per_unit     :: SFT_MVAL sft
    -- FDA
    , settled_flow_rate_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (SubscriptionContract sft)

-- * Monetary unit data

-- | Distribution publisher data.
--
-- Note: This is going to be very similar to the universal data. The separation is perhaps mostly for data tracking
--       reason. TODO: maybe really just use universal data for publisher side too?
data PublisherData sft = PublisherData
    { distributed_value :: UntappedValue (SFT_MVAL sft)
    , total_flow_rate   :: SFT_MVAL sft
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
