> -- -*- fill-column: 70; -*-

\ignore{
\begin{code}
{-# LANGUAGE TypeFamilyDependencies #-}
module Money.Theory.MoneyMedium
    -- $intro
    --
    ( NondetSeqMoneyToken (..)
    , MoneyNote (..)
    , NondetSeqMoneyNotes (..)
    ) where

import Money.Theory.MoneyDistribution
import Money.Theory.FinancialContract
import Money.Theory.PaymentExecutionEnvironment
\end{code}
}

\begin{haddock}
\begin{code}
{- $intro

Here are some toy models for non-deterministic sequential money tokens
and money notes:

-}
\end{code}
\end{haddock}

\begin{code}
type Address = String

-- | Toy model for non-deterministic sequential money token.
class ( MoneyDistribution md
      , FinancialContract fc md
      , NondetSeqPaymentExecEnv tk md fc
      ) => NondetSeqMoneyToken tk md fc where
    -- | Customary interface for querying one's current account balance.
    balanceOf :: mval ~ MD_MVAL md
              => Address -> tk mval

    -- The rest would be just convenience interfaces for ~fcMInsert~

-- | A money note that is capable of encoding financial contract.
data MoneyNote md fc = ( MoneyDistribution md
                       , FinancialContract fc md
                       ) => FinancialContractNote md fc
                       | MoneytaryUnitNote md

type NoteID = String

-- | A toy model for non-deterministic sequential money notes execution
-- environment.
class ( MoneyDistribution md
      , FinancialContract fc md
      , NondetSeqPaymentExecEnv env md fc
      ) => NondetSeqMoneyNotes env md fc where
    -- | Find note by its ID. This should be used by ~fc~ to rehydrate
    --   the its references to the notes.
    findNote :: note ~ MoneyNote md fc
             => NoteID -> env note

    -- | Customary interface for querying the note's current balance.
    balanceIn ::  ( mval ~ MD_MVAL md
                  , note ~ MoneyNote md fc
                  )
              => note -> env mval
\end{code}

It may seem tiny semantic differences between tokens and notes
execution environment, but it is on purpose. Their main difference lies
mainly in their ``user experience'' implementations.
