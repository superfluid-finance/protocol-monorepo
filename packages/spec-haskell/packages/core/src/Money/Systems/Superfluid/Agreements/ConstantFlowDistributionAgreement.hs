{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Instant distribution agreement.
--
-- It is instant transferring over an proportional distribution index
--
-- This module is typically imported using qualified name CFDA.
module Money.Systems.Superfluid.Agreements.ConstantFlowDistributionAgreement where

import           Data.Default
import           Data.Kind                                                          (Type)
import           GHC.Generics

import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow  as CFMUD
import           Money.Systems.Superfluid.Agreements.ProportionalDistributionCommon


-- * Contracts

data DistributionContract sft = DistributionContract
    { dc_updated_at     :: SFT_TS sft
    , dc_flow_rate      :: SFT_MVAL sft
    , dc_value_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (DistributionContract sft)

data SubscriptionContract sft = SubscriptionContract
    { sc_settled_value          :: UntappedValue (SFT_MVAL sft)
    , sc_settled_value_per_unit :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (SubscriptionContract sft)

-- * Monetary unit data

data PublisherData sft = PublisherData
    { pub_settled_at      :: SFT_TS sft
    , pub_settled_value   :: UntappedValue (SFT_MVAL sft)
    , pub_total_flow_rate :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (PublisherData sft)
type PublisherMonetaryUnitData sft = CFMUD.MonetaryUnitData (PublisherData sft) sft

type SubscriberData sft = ( DistributionContractBase sft, DistributionContract sft
                          , SubscriptionContractBase sft, SubscriptionContract sft)
type SubscriberMonetaryUnitData sft = CFMUD.MonetaryUnitData (SubscriberData sft) sft

instance SuperfluidTypes sft => CFMUD.MonetaryUnitLenses (PublisherData sft) sft where
    settledAt          = $(field 'pub_settled_at)
    settledValue       = $(field 'pub_settled_value)
    netFlowRate        = $(field 'pub_total_flow_rate)

instance SuperfluidTypes sft => CFMUD.MonetaryUnitLenses (SubscriberData sft) sft where
    settledAt     = readOnlyLens
        (\( _
          , _
          , SubscriptionContractBase { sub_settled_at = t }
          , _) -> t)

    netFlowRate   = readOnlyLens
        (\( DistributionContractBase { total_unit     = tu }
          , DistributionContract { dc_flow_rate       = dcfr }
          , SubscriptionContractBase { sub_owned_unit = u }
          , _
          ) -> floor $ fromIntegral dcfr * u / tu )

    settledValue = readOnlyLens
        (\( DistributionContractBase { total_unit            = tu}
          , DistributionContract { dc_value_per_unit         = vpu
                                 , dc_flow_rate              = dcfr
                                 , dc_updated_at             = t_dc
                                 }
          , SubscriptionContractBase { sub_owned_unit        = u
                                     , sub_settled_at        = t_sc
                                     }
          , SubscriptionContract { sc_settled_value          = UntappedValue sv
                                 , sc_settled_value_per_unit = svpu
                                 }
          ) -> let vpuΔ = floor $ fromIntegral dcfr * fromIntegral (t_dc - t_sc) / tu
               in  UntappedValue $ sv + floor (u * fromIntegral (vpu - svpu - vpuΔ)))

-- * Publisher Operations

newtype PublisherOperation sft = UpdateDistributionFlowRate (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (PublisherOperation sft) sft where
    applyAgreementOperation (UpdateDistributionFlowRate dcfr') (PublisherContract dcBase dc) t' = let
        dc'  = dc { dc_updated_at = t'
                  , dc_value_per_unit = vpu + vpuΔ
                  , dc_flow_rate = dcfr'
                  }
        aorΔ  = PublisherOperationResultF
                (def & set CFMUD.settledAt t'
                     & set CFMUD.netFlowRate (dcfr - dcfr') -- reverse sign for outgoing flow
                     & set CFMUD.settledValue def
                )
        in (PublisherContract dcBase dc', fmap CFMUD.MkMonetaryUnitData aorΔ)
        where DistributionContractBase { total_unit    = tu
                                       } = dcBase
              DistributionContract { dc_updated_at     = t_dc
                                   , dc_value_per_unit = vpu
                                   , dc_flow_rate      = dcfr
                                   } = dc
              -- FIXME can be a black hole due to tu == 0 or precision error
              --       need to communicate the actual distribution flow rate instead
              settledΔ = dcfr * fromIntegral (t' - t_dc)
              vpuΔ = if tu /= 0 then floor $ fromIntegral settledΔ / tu else 0

    data AgreementContract (PublisherOperation sft) = PublisherContract
        (DistributionContractBase sft) (DistributionContract sft)
    data AgreementOperationResultF (PublisherOperation sft) elem = PublisherOperationResultF elem -- publisher amud
        deriving stock (Functor, Foldable, Traversable)
    type AgreementMonetaryUnitDataInOperation (PublisherOperation sft) = PublisherMonetaryUnitData sft

type PublisherContract :: Type -> Type
type PublisherContract sft = AgreementContract (PublisherOperation sft)

-- * Subscriber Operations

data SubscriberOperation sft = SettleSubscription

instance SuperfluidTypes sft => AgreementOperation (SubscriberOperation sft) sft where
    applyAgreementOperation SettleSubscription (SubscriberContract (dcBase, dc, scBase, sc)) t' = let
        settledΔ = dcfr * fromIntegral (t' - t_dc)
        vpuΔ'    = if tu /= 0 then floor $ fromIntegral settledΔ / tu else 0

        vpu' = vpu_i + vpuΔ'
        dc'  = dc { dc_updated_at = t'
                  , dc_value_per_unit = vpu'
                  }

        svΔ = floor $ fromIntegral (vpu' - svpu) * u
        sc' = sc { sc_settled_value = UntappedValue $ sv + svΔ
                 , sc_settled_value_per_unit = vpu'
                 }

        aorΔ = def & set CFMUD.settledAt t'

        in ( SubscriberContract (dcBase, dc', scBase, sc')
           , fmap CFMUD.MkMonetaryUnitData $ SubscriberOperationPartiesF aorΔ)

        where DistributionContractBase { total_unit            = tu
                                       } = dcBase
              DistributionContract { dc_updated_at             = t_dc
                                   , dc_value_per_unit         = vpu_i
                                   , dc_flow_rate              = dcfr
                                   } = dc
              SubscriptionContractBase { sub_owned_unit        = u
                                   } = scBase
              SubscriptionContract { sc_settled_value          = UntappedValue sv
                                   , sc_settled_value_per_unit = svpu
                                   } = sc
              -- π' a t = netValueOfRTB $ balanceProvidedByAgreement a t

    data AgreementContract (SubscriberOperation sft) = SubscriberContract (SubscriberData sft)
    data AgreementOperationResultF (SubscriberOperation sft) elem = SubscriberOperationPartiesF elem
        deriving stock (Functor, Foldable, Traversable)
    type AgreementMonetaryUnitDataInOperation (SubscriberOperation sft) = PublisherMonetaryUnitData sft

type SubscriberContract :: Type -> Type
type SubscriberContract sft = AgreementContract (SubscriberOperation sft)
