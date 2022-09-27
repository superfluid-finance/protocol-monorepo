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

Financial contract is the execution context for payment primitives, including their execution conditions,
timing\footnote{Timing is a type of condition of which current system time is a factor} and execution order.

Inspired by the \textit{technique of composing financial contracts} demonstrated in \cite{peyton2000composing}, we
define the type class for financial contract as follows:

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

Because \textit{(md, ctx)} is monoidal too, itt is then possible to build a combinatorial libary of financial contracts
that can be used to construct larger and more complex financial contracts.
