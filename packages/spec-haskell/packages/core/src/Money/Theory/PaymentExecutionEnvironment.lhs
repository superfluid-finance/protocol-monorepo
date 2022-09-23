\ignore{
\begin{code}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Money.Theory.PaymentExecutionEnvironment where

import Money.Theory.MoneyDistribution
import Data.Kind (Type)
\end{code}
}

The purpose of a payment execution environment is to perform the actual payment primitives taking into account their
conditions, timing and order.

\subsubsection{Composing Financial Contracts}

Inspired by the \textit{technique of composing financial contracts} demonstrated in \cite{peyton2000composing}, we first
define a type class for financial contract:

\begin{code}
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

Additionally, let's define \textit{FinancialContractSet} be the set of all financial contracts in the payment system.

\begin{code}
class ( MoneyDistribution md
      , FinancialContract fc md
      , Monad (FCSET_MONAD fcSet)
      ) => FinancialContractSet fcSet fc md | fcSet -> fc where
    -- | Monadically select a financial contract from the set.
    selectFc :: m ~ FCSET_MONAD fcSet
             => fcSet -> m fc

    -- | Indexed monad type for the set. It could provide the side effect for
    --   ~fcSelect~ if it is necessary (such as randomly or depending on the environment).
    type family FCSET_MONAD fcSet = (m :: Type -> Type) | m -> fcSet
\end{code}

\subsubsection{Non-Deterministic Sequential Model}

A non-deterministic sequential execution environment model that can support conditional payment
primitives\footnote{Timing is a type of condition of which current system time is a factor} then can be defined as
follows:

\begin{code}
-- | Non-deterministically and sequentially execute payment in the payment system.
execPaymentNonDetSeq :: ( MoneyDistribution md
                        , FinancialContract fc md
                        , FinancialContractSet fcSet fc md
                        , Timestamp t
                        , ctx ~ MD_CTX md
                        , m ~ FCSET_MONAD fcSet
                        )
                     => md -> (fcSet, ctx) -> t -> m (md, ctx)
execPaymentNonDetSeq md (fcSet, ctx) t = selectFc fcSet >>= \fc -> return $
    if fcPred fc (md, ctx) t
    then fcExec fc (md, ctx) t
    else (mempty, ctx)
\end{code}

\subsubsection{Deterministic Execution Order}

\subsubsection{Concurrency}

\subsubsection{Risks & Solvency}
