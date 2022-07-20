{-# LANGUAGE DeriveAnyClass #-}


module Money.Systems.Superfluid.Indexes.ProportionalDistributionIndexes where

import           Data.Default
import           GHC.Generics

import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement as ITA


-- * Common Data Structures

data PublisherData sft ctype = PublisherData
    { total_unit          :: SFT_FLOAT sft
    , total_content_value :: ctype
    } deriving (Generic)
deriving instance (SuperfluidTypes sft, Default ctype) => Default (PublisherData sft ctype)

data SubscriberData sft ctype = SubscriberData
    { publisher_data :: PublisherData sft ctype
    , owned_unit     :: SFT_FLOAT sft
    } deriving (Generic)
deriving instance (SuperfluidTypes sft, Default ctype) => Default (SubscriberData sft ctype)

-- * ITA
--

data ITAMonetaryUnitLens sft = ITAMonetaryUnitData
    { ita_published_index    :: PublisherData  sft (SFT_MVAL sft)
    , ita_subscribed_indexes :: [SubscriberData sft (SFT_MVAL sft)]
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (ITAMonetaryUnitLens sft)

instance SuperfluidTypes sft => ITA.MonetaryUnitLens (ITAMonetaryUnitLens sft) sft where
    untappedValue = lens
        -- lens getter: subscribed value
        (\ITAMonetaryUnitData { ita_subscribed_indexes = subs } -> UntappedValue $
            foldr (+) def $ map
            (\(SubscriberData p u) -> ceiling $
                 let total_value = fromIntegral (total_content_value p)
                 in  total_value * u / (total_unit p)
            ) subs)
        -- lens setter: to publish value
        (\amud@(ITAMonetaryUnitData {ita_published_index = pub }) (UntappedValue amount) ->
             let tcv = total_content_value pub
             in  amud { ita_published_index = pub
                          { total_content_value = tcv + amount }} )
type ITAMonetaryUnitData sft = ITA.MonetaryUnitData (ITAMonetaryUnitLens sft) sft

-- not much to look into really
data ITAContractLens sft = ITAContractData deriving (Generic, Default)

instance SuperfluidTypes sft => ITA.ContractLens (ITAContractLens sft) sft
type ITAContractData sft = ITA.ContractData (ITAContractLens sft) (ITAMonetaryUnitLens sft) sft
