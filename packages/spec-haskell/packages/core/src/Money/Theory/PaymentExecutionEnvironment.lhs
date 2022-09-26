\ignore{
\begin{code}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Money.Theory.PaymentExecutionEnvironment
    -- $intro
    --
    ( FinancialContract (..)
    , FinancialContractSet (..)
    , stepNextPaymentNonDetSeq
    ) where

import Money.Theory.MoneyDistribution
import Money.Theory.FinancialContract
import Data.Kind (Type)
\end{code}
}

\begin{haddock}
\begin{code}
{- $intro

Here are some simplified models for payment execution environment.

-}
\end{code}
\end{haddock}

Let's first define \textit{FinancialContractSet} be the set of all financial contracts in the payment system.

\begin{code}
class ( MoneyDistribution md
      , FinancialContract fc md
      , Monad (FCSET_MONAD fcSet)
      ) => FinancialContractSet fcSet fc md | fcSet -> fc where
    -- | Monadically select a financial contract from the set.
    selectFc :: m ~ FCSET_MONAD fcSet
             => fcSet -> m fc

    -- | Indexed monad type for the set. It encodes the type of
    --   side effect used in ~selectFc~.
    type family FCSET_MONAD fcSet = (m :: Type -> Type) | m -> fcSet
\end{code}

Here the associated type synonym \textit{FCSET\_MONAD fcSet} is a Monad, where diferent side effects for
\textit{fcSelect} can be encoded, for instances:

\begin{itemize}
\item \textit{Idendity monad} - there is no side effect, \textit{selectFc} then must be deterministic.
\item \textit{IO monad} - a computation that involve input/output with the world.
\end{itemize}

For more generalized interface to computation, arrows could be used instead \cite{hughes2000generalising}.

\paragraph{Sequential Model}

A naive sequential execution environment model that supports conditional payment primitives\footnote{Timing is a type of
condition of which current system time is a factor} then can be defined as follows:

\begin{code}
-- | Naive version of step function in a sequential payment system.
stepNextPaymentNonDetSeq :: ( MoneyDistribution md
                            , FinancialContract fc md
                            , FinancialContractSet fcSet fc md
                            , Timestamp t
                            , ctx ~ MD_CTX md
                            , m ~ FCSET_MONAD fcSet
                            )
                         =>  (md, ctx) -> fcSet -> t -> m (md, ctx)
stepNextPaymentNonDetSeq (md, ctx) fcSet t = selectFc fcSet >>= \fc -> return $
    if fcPred fc (md, ctx) t
    then (md, ctx) <> fcExec fc (md, ctx) t
    else (md, ctx)
\end{code}

\paragraph{Non-deterministic Concurrent Model}

\paragraph{Deterministic Concurrent Model}

\paragraph{Payment System Solvency}
