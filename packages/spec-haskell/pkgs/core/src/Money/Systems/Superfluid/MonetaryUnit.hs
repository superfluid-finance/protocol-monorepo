{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.MonetaryUnit where

import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes
--
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistribution.ConstantFlowDistributionAgreement as CFDA
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistribution.InstantDistributionAgreement      as IDA
import qualified Money.Systems.Superfluid.Agreements.Universal.ConstantFlowAgreement                            as CFA
import qualified Money.Systems.Superfluid.Agreements.Universal.DecayingFlowAgreement                            as DFA
import qualified Money.Systems.Superfluid.Agreements.Universal.InstantTransferAgreement                         as ITA
import qualified Money.Systems.Superfluid.Agreements.Universal.MinterAgreement                                  as MINTA
--
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex                              as PDIDX
import qualified Money.Systems.Superfluid.Agreements.UniversalIndex                                             as UIDX


-- | Monetary unit type class.
--
-- It represents the Superfluid take on the monetary unit in the theory of money distribution.
class SuperfluidSystemTypes sft => MonetaryUnit mu sft | mu -> sft where

    -- * Nomenclature:
    --
    -- MINTA = Minter Agreement over the Universal Index
    -- ITA   = Instant Transfer Agreement over the Universal Index
    -- CFA   = Constant Flow Agreement over the Universal Index
    -- DFA   = Decaying Flow Agreement over the Universal Index
    -- IDA   = Instant Distribution Agreement over a Proportional Distribution Index
    -- FDA   = Constant Flow Agreement over a Proportional Distribution Index
    -- DFDA  = Decaying Flow Agreement over a Proportional Distribution Index

    -- * Universal Index Agreement Operations
    --

    -- | Getter for the lenses of monetary unit data in the universal index.
    universalData :: SimpleGetter mu (UIDX.UniversalData sft)

    -- | Lens for Minter data.
    minterMonetaryUnitData :: Lens' mu (MINTA.MonetaryUnitData sft)

    -- | Lens for ITA data.
    itaMonetaryUnitData :: Lens' mu (ITA.MonetaryUnitData sft)

    -- | Lens for CFA data.
    cfaMonetaryUnitData :: Lens' mu (CFA.MonetaryUnitData sft)

    -- | Lens for DFA data.
    dfaMonetaryUnitData :: Lens' mu (DFA.MonetaryUnitData sft)

    -- * Proportional Distribution Index Agreement Operations
    --

    -- | Getter for the publisher lenses of monetary unit data in the proportional distribution (PD) index.
    pdPublisherData :: SimpleGetter mu (PDIDX.PublisherData sft)

    -- | Lens for the publisher IDA data.
    idaPublisherMonetaryUnitData :: Lens' mu (IDA.PublisherMonetaryUnitData sft)

    -- | Getter for the list of subscriber IDA data.
    idaSubscriberMonetaryUnitDataList :: SimpleGetter mu [IDA.SubscriberMonetaryUnitData sft]

    -- | Lens for the publisher CFDA data.
    cfdaPublisherMonetaryUnitData :: Lens' mu (CFDA.PublisherMonetaryUnitData sft)

    -- | Getter for the list of subscriber CFDA data.
    cfdaSubscriberMonetaryUnitDataList :: SimpleGetter mu [CFDA.SubscriberMonetaryUnitData sft]

    -- | List of monetary unit data warapped in ~AnyType~.
    monetaryUnitDataList :: mu -> [SFT_ANY_MUD sft]

    -- | Calculate the real time balance of an monetary unit at a given time.
    balanceOfAt :: mu -> SFT_TS sft -> SFT_RTB sft
    balanceOfAt mu t = foldr
        ((<>) . flip balanceProvided t)
        mempty
        (monetaryUnitDataList mu)

    -- | Sum the real time balances of a list of accounts at a given time.
    sumBalancesAt :: [mu] -> SFT_TS sft -> SFT_RTB sft
    sumBalancesAt alist t = foldr ((<>) . (`balanceOfAt` t)) mempty alist
