\ignore{
\begin{code}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Money.Theory.FinancialContract
    -- $intro
    --
    ( FinancialContract (..)
    ) where

import Money.Theory.MoneyDistribution
\end{code}
}

\begin{haddock}
\begin{code}
{- $intro

Inspired by the \textit{technique of composing financial contracts} demonstrated in \cite{peyton2000composing}, we first
define a type class for financial contract:

-}
\end{code}
\end{haddock}

\begin{code}
-- | Financial contract inspired by \\cite{peyton2000composing}.
class MoneyDistribution md => FinancialContract fc md | fc -> md where
    -- | Predicate of the execution condition of a financial contract.
    fcPred :: ( ctx ~ MD_CTX md
              , Timestamp t
              )
           => fc -> (md, ctx) -> t -> Bool

    -- | Execute the payment primitives encoded in the financial contract.
    fcExec :: ( ctx ~ MD_CTX md
              , Timestamp t
              )
           => fc -> (md, ctx) -> t -> (md, ctx)
\end{code}
