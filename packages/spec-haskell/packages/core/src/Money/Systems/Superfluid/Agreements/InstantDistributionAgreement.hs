{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Instant distribution agreement.
--
-- It is instant transferring over an proportional distribution index
--
-- This module is typically imported using qualified name .
module Money.Systems.Superfluid.Agreements.InstantDistributionAgreement where

import           Data.Coerce
import           Data.Default
import           Data.Kind                                                          (Type)
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantValue  as IVMUD
import           Money.Systems.Superfluid.Agreements.ProportionalDistributionCommon


-- * Contracts

data DistributionContract sft = DistributionContract
    { dc_value_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (DistributionContract sft)

data SubscriptionContract sft = SubscriptionContract
    { sc_settled_value          :: UntappedValue (SFT_MVAL sft)
    , sc_settled_value_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (SubscriptionContract sft)

-- * Monetary unit data

data PublisherData sft = PublisherData
    { pub_settled_value :: UntappedValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (PublisherData sft)
type PublisherMonetaryUnitData sft = IVMUD.MonetaryUnitData (PublisherData sft) sft

type SubscriberData sft = ( DistributionContractBase sft, DistributionContract sft
                          , SubscriptionContractBase sft, SubscriptionContract sft)
type SubscriberMonetaryUnitData sft = IVMUD.MonetaryUnitData (SubscriberData sft) sft

instance SuperfluidTypes sft => IVMUD.MonetaryUnitLenses (PublisherData sft) sft where
    untappedValue = $(field 'pub_settled_value)

instance SuperfluidTypes sft => IVMUD.MonetaryUnitLenses (SubscriberData sft) sft where
    untappedValue = readOnlyLens
        -- lens getter: subscribed value
        (\( _
          , DistributionContract { dc_value_per_unit         = vpu
                                 }
          , SubscriptionContractBase { sub_owned_unit        = u
                                     }
          , SubscriptionContract { sc_settled_value          = UntappedValue sv
                                 , sc_settled_value_per_unit = svpu
                                 }
          ) -> UntappedValue $ sv + floor (u * fromIntegral (vpu - svpu)))

-- * Publisher Operations

newtype PublisherOperation sft = Distribute (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (PublisherOperation sft) sft where
    applyAgreementOperation (Distribute amount) (PublisherContract dcBase dc) _ = let
        vpuΔ = fromIntegral amount / tu

        dc'  = dc { dc_value_per_unit = floor (fromIntegral vpu + vpuΔ) }

        aorΔ  = PublisherOperationResultF
                  (def & set IVMUD.untappedValue (coerce (- amount)))

        in (PublisherContract dcBase dc', fmap IVMUD.MkMonetaryUnitData aorΔ)

        where DistributionContractBase { total_unit = tu} = dcBase
              DistributionContract { dc_value_per_unit = vpu } = dc

    data AgreementContract (PublisherOperation sft) = PublisherContract
        (DistributionContractBase sft) (DistributionContract sft)
        deriving (Generic)
    data AgreementOperationResultF (PublisherOperation sft) elem = PublisherOperationResultF elem -- publisher mud
        deriving stock (Functor, Foldable, Traversable)
    type MonetaryUnitDataInOperation (PublisherOperation sft) = PublisherMonetaryUnitData sft

type PublisherContract :: Type -> Type
type PublisherContract sft = AgreementContract (PublisherOperation sft)

deriving instance SuperfluidTypes sft => Default (PublisherContract sft)

-- * Subscriber Operations

data SubscriberOperation sft = SettleSubscription

instance SuperfluidTypes sft => AgreementOperation (SubscriberOperation sft) sft where
    applyAgreementOperation SettleSubscription (SubscriberContract (dcBase, dc, scBase, sc)) _ = let
        svΔ = floor $ fromIntegral (vpu - svpu) * u

        sc'  = sc { sc_settled_value = UntappedValue $ sv + svΔ
                  , sc_settled_value_per_unit = vpu
                  }

        in (SubscriberContract (dcBase, dc, scBase, sc'), SubscriberOperationPartiesF)

        where DistributionContract { dc_value_per_unit         = vpu
                                   } = dc
              SubscriptionContractBase { sub_owned_unit        = u
                                       } = scBase
              SubscriptionContract { sc_settled_value          = UntappedValue sv
                                   , sc_settled_value_per_unit = svpu
                                   } = sc

    data AgreementContract (SubscriberOperation sft) = SubscriberContract (SubscriberData sft)
        deriving (Generic)
    data AgreementOperationResultF (SubscriberOperation sft) elem = SubscriberOperationPartiesF
    type MonetaryUnitDataInOperation (SubscriberOperation sft) = NullMonetaryUnitData sft

type SubscriberContract :: Type -> Type
type SubscriberContract sft = AgreementContract (SubscriberOperation sft)

deriving instance SuperfluidTypes sft => Default (SubscriberContract sft)
