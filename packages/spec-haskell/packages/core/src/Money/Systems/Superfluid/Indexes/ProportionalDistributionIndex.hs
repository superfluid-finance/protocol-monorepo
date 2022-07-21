{-# LANGUAGE DeriveAnyClass #-}


module Money.Systems.Superfluid.Indexes.ProportionalDistributionIndex where

import           Data.Default
import           GHC.Generics

import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement as ITA


data ProportionalDistributionIndex sft = ProportionalDistributionIndex
    { total_unit              :: SFT_FLOAT sft
    , total_distributed_value :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (ProportionalDistributionIndex sft)

data ProportionalDistributionSubscription sft = ProportionalDistributionSubscription
    { index      :: ProportionalDistributionIndex sft
    , owned_unit :: SFT_FLOAT sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (ProportionalDistributionSubscription sft)

-- * IDA is IDA over the Proportional Distribution Index
--

instance SuperfluidTypes sft => ITA.MonetaryUnitLenses (ProportionalDistributionIndex sft) sft where
    untappedValue = lens
        -- lens getter: published value + subscribed value
        (\ProportionalDistributionIndex { total_distributed_value = tdv } -> UntappedValue $ tdv)
        -- lens setter: add to publish value
        (\index (UntappedValue amount) ->
             let tdv = total_distributed_value index
             in  index { total_distributed_value = tdv + amount })

instance SuperfluidTypes sft => ITA.MonetaryUnitLenses (ProportionalDistributionSubscription sft) sft where
    untappedValue = readOnlyLens
        -- lens getter: subscribed value
        (\ProportionalDistributionSubscription { index, owned_unit = u } -> UntappedValue $ floor $
            let total_value = fromIntegral (total_distributed_value index)
            in  total_value * u / (total_unit index))

type IDAPublisherMonetaryUnitData sft  = ITA.MonetaryUnitData (ProportionalDistributionIndex sft) sft
type IDASubscriberMonetaryUnitData sft = ITA.MonetaryUnitData (ProportionalDistributionSubscription sft) sft

type IDAPublisherContractData sft = ITA.ContractData (ProportionalDistributionIndex sft) sft
type IDASubscriberContractData sft = ITA.ContractData (ProportionalDistributionSubscription sft) sft
