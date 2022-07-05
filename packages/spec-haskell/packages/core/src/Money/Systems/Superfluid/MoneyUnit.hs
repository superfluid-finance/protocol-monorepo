{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.MoneyUnit
    ( MoneyUnit (..)
    , balanceOfAt
    , sumBalancesAt
    ) where

import           Data.Default                                                     (Default (def))
import           Data.Kind                                                        (Type)

import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
import           Money.Systems.Superfluid.Concepts                                (SuperfluidTypes (..))


-- | MoneyUnit type class.
class SuperfluidTypes sft => MoneyUnit acc sft | acc -> sft where
    --
    -- Polymorphic agreement account data functions
    --

    type AnyAgreementAccountData acc :: Type

    agreementsOf :: acc -> [AnyAgreementAccountData acc]

    providedBalanceByAnyAgreement :: acc -> AnyAgreementAccountData acc -> SFT_TS sft -> SFT_RTB sft

    --
    -- Specialized agreement account data functions
    --

    viewTBA :: acc -> TBA.TBAAccountData sft
    setTBA :: acc -> TBA.TBAAccountData sft -> SFT_TS sft -> acc

    viewCFA :: acc -> CFA.CFAAccountData sft
    setCFA :: acc -> CFA.CFAAccountData sft -> SFT_TS sft -> acc

    viewDFA ::  acc -> DFA.DFAAccountData sft
    setDFA :: acc -> DFA.DFAAccountData sft -> SFT_TS sft -> acc

balanceOfAt :: (SuperfluidTypes sft, MoneyUnit acc sft) => acc -> SFT_TS sft -> SFT_RTB sft
balanceOfAt account t = foldr
    ((+) . (\a -> providedBalanceByAnyAgreement account a t))
    def
    (agreementsOf account)

sumBalancesAt :: (SuperfluidTypes sft, MoneyUnit acc sft) => [acc] -> SFT_TS sft -> SFT_RTB sft
sumBalancesAt alist t = foldr ((+) . (`balanceOfAt` t)) def alist
