{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.MonetaryUnit
    ( MonetaryUnit (..)
    , balanceOfAt
    , sumBalancesAt
    ) where

import           Data.Kind                                                    (Type)

import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement    as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement    as DFA
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement as ITA
import qualified Money.Systems.Superfluid.Agreements.MinterAgreement          as MINTA
--
import qualified Money.Systems.Superfluid.Indexes.UniversalIndexes            as UIDX

-- | MonetaryUnit type class.
class SuperfluidTypes sft => MonetaryUnit mu sft | mu -> sft where
    -- * Polymorphic agreement account data functions
    --

    type AnyAgreementMonetaryUnitData mu :: Type

    providedBalanceByAnyAgreement        :: mu -> AnyAgreementMonetaryUnitData mu -> SFT_TS sft -> SFT_RTB sft

    agreementsOf                         :: mu -> [AnyAgreementMonetaryUnitData mu]

    -- * Lenses of agreement monetary unit data
    --

    -- | Lens for MINTA data in the mu.
    mintaMonetaryUnitData :: Lens' mu (UIDX.MINTAMonetaryUnitData sft)
    -- | Lens for the MINTA data itself.
    mintaMonetaryUnitLenses :: SimpleGetter mu (UIDX.MINTAMonetaryUnitLenses sft)
    mintaMonetaryUnitLenses = to $ \m -> MINTA.getMonetaryUnitLenses (m^.mintaMonetaryUnitData)

    -- | Lens for ITA data in the mu.
    itaMonetaryUnitData   :: Lens' mu (UIDX.ITAMonetaryUnitData sft)
    -- | Lens for the ITA data itself.
    itaMonetaryUnitLenses :: SimpleGetter mu (UIDX.ITAMonetaryUnitLenses sft)
    itaMonetaryUnitLenses = to $ \m -> ITA.getMonetaryUnitLenses (m^.itaMonetaryUnitData)

    -- | Lens for CFA data in the mu.
    cfaMonetaryUnitData   :: Lens' mu (UIDX.CFAMonetaryUnitData sft)
    -- | Getter of the lens into the CFA data.
    cfaMonetaryUnitLenses :: SimpleGetter mu (UIDX.CFAMonetaryUnitLenses sft)
    cfaMonetaryUnitLenses = to $ \m -> CFA.getMonetaryUnitLenses (m^.cfaMonetaryUnitData)

    -- | Lens for DFA data in the mu.
    dfaMonetaryUnitData   :: Lens' mu (UIDX.DFAMonetaryUnitData sft)
    -- | Getter of the lens into the DFA data.
    dfaMonetaryUnitLenses :: SimpleGetter mu (UIDX.DFAMonetaryUnitLenses sft)
    dfaMonetaryUnitLenses = to $ \m -> DFA.getMonetaryUnitLenses (m^.dfaMonetaryUnitData)

balanceOfAt :: (SuperfluidTypes sft, MonetaryUnit mu sft) => mu -> SFT_TS sft -> SFT_RTB sft
balanceOfAt account t = foldr
    ((<>) . (\a -> providedBalanceByAnyAgreement account a t))
    mempty
    (agreementsOf account)

sumBalancesAt :: (SuperfluidTypes sft, MonetaryUnit mu sft) => [mu] -> SFT_TS sft -> SFT_RTB sft
sumBalancesAt alist t = foldr ((<>) . (`balanceOfAt` t)) mempty alist
