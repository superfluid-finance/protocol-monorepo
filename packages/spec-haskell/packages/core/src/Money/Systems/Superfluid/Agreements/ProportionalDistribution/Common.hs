{-# LANGUAGE DeriveAnyClass #-}

module Money.Systems.Superfluid.Agreements.ProportionalDistribution.Common where

import           Data.Default
import           GHC.Generics

import           Money.Systems.Superfluid.SystemTypes

data DistributionContractBase sft = DistributionContractBase
    { total_unit :: SFT_FLOAT sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (DistributionContractBase sft)

data SubscriptionContractBase sft = SubscriptionContractBase
    { sub_owned_unit :: SFT_FLOAT sft
    , sub_settled_at :: SFT_TS sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (SubscriptionContractBase sft)
