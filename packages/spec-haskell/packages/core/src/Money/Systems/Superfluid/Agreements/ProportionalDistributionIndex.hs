{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex where

import           Data.Coerce
import           Data.Default
import           GHC.Generics

import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantTransfer as ITMUD

-- Index data

data DistributionContract sft = DistributionContract
    { total_unit         :: SFT_FLOAT sft
    , value_per_unit     :: SFT_MVAL sft
    , flow_rate_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (DistributionContract sft)
instance SuperfluidTypes sft => AgreementContractData (DistributionContract sft) sft

data SubscriptionContract sft = SubscriptionContract
    { distribution_contract      :: DistributionContract sft
    , owned_unit                 :: SFT_FLOAT sft
    , settled_value              :: UntappedValue (SFT_MVAL sft)
    , settled_value_per_unit     :: SFT_MVAL sft
    , settled_flow_rate_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (SubscriptionContract sft)
instance SuperfluidTypes sft => AgreementContractData (SubscriptionContract sft) sft

data PublisherData sft = PublisherData
    { distributed_value :: UntappedValue (SFT_MVAL sft)
    , total_flow_rate   :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (PublisherData sft)
type IDAPublisherMonetaryUnitData sft = ITMUD.MonetaryUnitData (PublisherData sft) sft

type IDASubscriberMonetaryUnitData sft = ITMUD.MonetaryUnitData (SubscriptionContract sft) sft

-- * IDA is IDA over the Proportional Distribution Index
--

instance SuperfluidTypes sft => ITMUD.MonetaryUnitLenses (PublisherData sft) sft where
    untappedValue = $(field 'distributed_value)

-- | The contract is the data. It is not scalable.
instance SuperfluidTypes sft => ITMUD.MonetaryUnitLenses (SubscriptionContract sft) sft where
    untappedValue = readOnlyLens
        -- lens getter: subscribed value
        (\SubscriptionContract { settled_value_per_unit = svpu
                               , settled_value = sv
                               , distribution_contract = dc
                               , owned_unit = u
                               } -> (+) sv $ UntappedValue $ floor $
            let DistributionContract { value_per_unit = vpu } = dc
            in  u * fromIntegral (vpu - svpu))

data IDAPublisherOperation sft = Distribute (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (IDAPublisherOperation sft)
    (DistributionContract sft) (IDAPublisherMonetaryUnitData sft) sft where
    data AgreementOperationPartiesF (IDAPublisherOperation sft) elem = IDAOPublisherOperationPartiesF elem
        deriving stock (Functor, Foldable, Traversable)

    applyAgreementOperation (Distribute amount) acd _ = let
        acd'  = acd { value_per_unit = floor (fromIntegral p + delta) }
        aopsΔ = fmap ITMUD.MkMonetaryUnitData (IDAOPublisherOperationPartiesF
                    (def & set ITMUD.untappedValue (coerce (- amount))))
        in (acd', aopsΔ)
        where DistributionContract { total_unit = tu, value_per_unit = p } = acd
              delta = fromIntegral amount / tu

data IDASubscriberOperation sft = Subscribe   (SFT_FLOAT sft) |
                                  Unsubscribe

instance SuperfluidTypes sft => AgreementOperation (IDASubscriberOperation sft)
    (SubscriptionContract sft) (IDASubscriberMonetaryUnitData sft) sft where
    data AgreementOperationPartiesF (IDASubscriberOperation sft) elem = IDASubscriberOperationPartiesF elem
        deriving stock (Functor, Foldable, Traversable)

    applyAgreementOperation (Subscribe unit) acd _ = let
        acd'  = acd { distribution_contract = dc { total_unit = tu + unit }
                    , owned_unit = u + unit
                    , settled_value_per_unit = vpu
                    , settled_value = UntappedValue sv'
                    }
        aopsΔ = fmap ITMUD.MkMonetaryUnitData (IDASubscriberOperationPartiesF def)
        in (acd', aopsΔ)
        where SubscriptionContract { distribution_contract = dc
                                   , owned_unit = u
                                   , settled_value_per_unit = svpu
                                   , settled_value = UntappedValue sv
                                   } = acd
              DistributionContract { total_unit = tu, value_per_unit = vpu } = dc
              sv' = floor (fromIntegral sv + fromIntegral (vpu - svpu) * u)

-- type IDAPublisherMonetaryUnitData sft  = ITMUD.MonetaryUnitData (SubscriberData sft) sft
