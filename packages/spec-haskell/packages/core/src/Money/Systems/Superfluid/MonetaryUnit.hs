{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.MonetaryUnit
    ( MonetaryUnit (..)
    , balanceOfAt
    , sumBalancesAt
    ) where

import           Data.Kind                                         (Type)
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Indexes.UniversalIndexes as UIDX

-- | MonetaryUnit type class.
class SuperfluidTypes sft => MonetaryUnit mu sft | mu -> sft where
    -- * Polymorphic agreement account data functions
    --

    type AnyAgreementMonetaryUnitData mu :: Type

    agreementsOf                         :: mu -> [AnyAgreementMonetaryUnitData mu]

    providedBalanceByAnyAgreement        :: mu -> AnyAgreementMonetaryUnitData mu -> SFT_TS sft -> SFT_RTB sft

    -- * Lens of agreement monetary unit data
    --

    itaMonetaryUnitData :: Lens'        mu (UIDX.ITAMonetaryUnitData sft)
    itaMonetaryUnitLens :: SimpleGetter mu (UIDX.ITAMonetaryUnitLens sft)

    cfaMonetaryUnitData :: Lens'        mu (UIDX.CFAMonetaryUnitData sft)
    cfaMonetaryUnitLens :: SimpleGetter mu (UIDX.CFAMonetaryUnitLens sft)

    dfaMonetaryUnitData :: Lens'        mu (UIDX.DFAMonetaryUnitData sft)
    dfaMonetaryUnitLens :: SimpleGetter mu (UIDX.DFAMonetaryUnitLens sft)

balanceOfAt :: (SuperfluidTypes sft, MonetaryUnit mu sft) => mu -> SFT_TS sft -> SFT_RTB sft
balanceOfAt account t = foldr
    ((<>) . (\a -> providedBalanceByAnyAgreement account a t))
    mempty
    (agreementsOf account)

sumBalancesAt :: (SuperfluidTypes sft, MonetaryUnit mu sft) => [mu] -> SFT_TS sft -> SFT_RTB sft
sumBalancesAt alist t = foldr ((<>) . (`balanceOfAt` t)) mempty alist
