{-# LANGUAGE DeriveAnyClass #-}

module Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex where

import           Data.Default
import           GHC.Generics


import           Money.Systems.Superfluid.Concepts

-- * Contract

data DistributionContract sft = DistributionContract
    { total_unit         :: SFT_FLOAT sft
    , value_per_unit     :: SFT_MVAL sft
    , flow_rate_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (DistributionContract sft)

data SubscriptionContract sft = SubscriptionContract
    { owned_unit                 :: SFT_FLOAT sft
    , settled_value              :: UntappedValue (SFT_MVAL sft)
    , settled_value_per_unit     :: SFT_MVAL sft
    , settled_flow_rate_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (SubscriptionContract sft)

-- * Monetary data

data PublisherData sft = PublisherData
    { distributed_value :: UntappedValue (SFT_MVAL sft)
    , total_flow_rate   :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (PublisherData sft)

data SubscriberData sft = SubscriberData
    { distribution_contract :: DistributionContract sft
    , subscription_contract :: SubscriptionContract sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (SubscriberData sft)
