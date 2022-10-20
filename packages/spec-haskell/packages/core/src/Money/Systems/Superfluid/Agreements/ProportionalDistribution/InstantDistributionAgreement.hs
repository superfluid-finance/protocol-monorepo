{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Instant distribution agreement.
--
-- It is instant transferring over an proportional distribution index
--
-- This module is typically imported using qualified name .
module Money.Systems.Superfluid.Agreements.ProportionalDistribution.InstantDistributionAgreement where

import           Data.Coerce
import           Data.Default
import           Data.Type.Any
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes
--
import           Money.Systems.Superfluid.Agreements.ProportionalDistribution.Common
import qualified Money.Systems.Superfluid.MonetaryUnitData.InstantValue              as IVMUD


-- * Contracts

data DistributionContract sft = DistributionContract
    { dc_value_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (DistributionContract sft)

data SubscriptionContract sft = SubscriptionContract
    { sc_settled_value          :: UntappedValue (SFT_MVAL sft)
    , sc_settled_value_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (SubscriptionContract sft)

type DistributionContractFull sft = (DistributionContractBase sft, DistributionContract sft)

type SubscriptionContractFull sft = (SubscriptionContractBase sft, SubscriptionContract sft)

type PublisherContract sft = DistributionContractFull sft

type SubscriberContract sft = (DistributionContractFull sft , SubscriptionContractFull sft)

-- * Publisher Monetary unit data

data PublisherData sft = PublisherData
    { pub_settled_value :: UntappedValue (SFT_MVAL sft)
    } deriving (Generic)

deriving instance SuperfluidSystemTypes sft => Default (PublisherData sft)

type PublisherMonetaryUnitData sft = IVMUD.MonetaryUnitData (PublisherData sft) sft
instance SuperfluidSystemTypes sft => SemigroupMonetaryUnitData (PublisherMonetaryUnitData sft) sft

-- * Subscriber Monetary unit data

type SubscriberData sft = SubscriberContract sft
type SubscriberMonetaryUnitData sft = IVMUD.MonetaryUnitData (SubscriberData sft) sft

instance SuperfluidSystemTypes sft => IVMUD.MonetaryUnitLenses (PublisherData sft) sft where
    untappedValue = $(field 'pub_settled_value)

instance SuperfluidSystemTypes sft => IVMUD.MonetaryUnitLenses (SubscriberData sft) sft where
    untappedValue = readOnlyLens
        -- lens getter: subscribed value
        (\(( _
           , DistributionContract { dc_value_per_unit         = vpu              }),
           ( SubscriptionContractBase { sub_owned_unit        = u                }
           , SubscriptionContract { sc_settled_value          = sv
                                  , sc_settled_value_per_unit = svpu             })
          ) -> sv + floor (u * fromIntegral (vpu - svpu)))

-- * Publisher Operations

instance SuperfluidSystemTypes sft => MonetaryUnitDataClass (PublisherContract sft) sft where

instance SuperfluidSystemTypes sft => AgreementContract (PublisherContract sft) sft where
    applyAgreementOperation (dcBase, dc) (Distribute amount) _ = let
        vpuΔ = fromIntegral amount / tu

        dc'  = dc { dc_value_per_unit = floor (fromIntegral vpu + vpuΔ) }

        mudsΔ = PublisherOperationOutputF
            (def & set IVMUD.untappedValue (coerce (- amount)))

        in ( (dcBase, dc')
           , fmap IVMUD.MkMonetaryUnitData mudsΔ)

        where DistributionContractBase { total_unit = tu} = dcBase
              DistributionContract { dc_value_per_unit = vpu } = dc

    functorizeAgreementOperationOutput p = fmap (mkAny p)

    data AgreementOperation (PublisherContract sft) = Distribute (SFT_MVAL sft)

    type AgreementOperationOutput (PublisherContract sft) = PublisherOperationOutput sft

    data AgreementOperationOutputF (PublisherContract sft) elem = PublisherOperationOutputF
        elem -- publisher
        deriving stock (Functor, Foldable, Traversable, Generic)

type PublisherOperationOutput sft = AgreementOperationOutputF (PublisherContract sft)
    (PublisherMonetaryUnitData sft)

instance SuperfluidSystemTypes sft => Default (PublisherOperationOutput sft)
instance SuperfluidSystemTypes sft => Monoid (PublisherOperationOutput sft) where mempty = def
instance SuperfluidSystemTypes sft => Semigroup (PublisherOperationOutput sft) where
    PublisherOperationOutputF a <> PublisherOperationOutputF a' = PublisherOperationOutputF (a <> a')

-- * Subscriber Operations

instance SuperfluidSystemTypes sft => MonetaryUnitDataClass (SubscriberContract sft) sft where
    balanceProvided = balanceProvided . IVMUD.MkMonetaryUnitData

instance SuperfluidSystemTypes sft => AgreementContract (SubscriberContract sft) sft where

    applyAgreementOperation ((dcBase, dc), (scBase, sc)) SettleSubscription _ = let
        svΔ = floor $ fromIntegral (vpu - svpu) * u

        sc'  = sc { sc_settled_value = sv + svΔ
                  , sc_settled_value_per_unit = vpu
                  }

        in (((dcBase, dc), (scBase, sc')), SubscriberOperationOutputF)

        where DistributionContract { dc_value_per_unit         = vpu
                                   } = dc
              SubscriptionContractBase { sub_owned_unit        = u
                                       } = scBase
              SubscriptionContract { sc_settled_value          = sv
                                   , sc_settled_value_per_unit = svpu
                                   } = sc

    functorizeAgreementOperationOutput _ _ = SubscriberOperationOutputF

    data AgreementOperation (SubscriberContract sft) = SettleSubscription

    type AgreementOperationOutput (SubscriberContract sft) = SubscriberOperationOutput sft

    data AgreementOperationOutputF (SubscriberContract sft) _ = SubscriberOperationOutputF
        deriving stock (Functor, Foldable, Traversable, Generic)

type SubscriberOperationOutput sft = AgreementOperationOutputF (SubscriberContract sft) ()

instance SuperfluidSystemTypes sft => Default (SubscriberOperationOutput sft)
instance SuperfluidSystemTypes sft => Monoid (SubscriberOperationOutput sft) where mempty = def
instance SuperfluidSystemTypes sft => Semigroup (SubscriberOperationOutput sft) where (<>) = const
