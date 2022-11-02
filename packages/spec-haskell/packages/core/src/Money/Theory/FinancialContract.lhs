> -- -*- fill-column: 70; -*-

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

A financial contract is the execution context for payment primitives,
including their execution conditions, timing\footnote{Timing is a type
of condition of which current system time is a factor}, and execution
order.

Inspired by the \textit{technique of composing financial contracts}
demonstrated in \cite{peyton2000composing}, we define the type class
for financial contracts as follows:

-}
\end{code}
\end{haddock}

\begin{code}
-- | Composable financial contracts.
class MoneyDistribution md => FinancialContract fc md | fc -> md where
    -- | Predicate of the execution condition of a financial contract.
    fcPred :: ( ctx ~ MD_CTX md
              , Timestamp t
              )
           => fc -> (md, ctx) -> t -> Bool

    -- | Execute the payment primitives encoded in the financial
    -- contract.
    fcExec :: ( ctx ~ MD_CTX md
              , Timestamp t
              )
           => fc -> (md, ctx) -> t -> ((md, ctx), fc)
\end{code}

We know that both $md$ and $ctx$ are constrained to be monoid, then
\textit{(md, ctx)} must be monoidal too. With this, it is possible to
build a combinatorial library of financial contracts that can be used
to construct more complex financial contracts.
