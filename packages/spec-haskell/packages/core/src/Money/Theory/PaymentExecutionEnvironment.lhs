\ignore{
\begin{code}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Money.Theory.PaymentExecutionEnvironment
    -- $intro
    --
    ( NondetSeqPaymentExecEnv (..)
    , TotallyOrderedFinancialContract
    , PartiallyOrderedFinancialContract
    , DetSeqPaymentExecEnv (..)
    ) where

import Money.Theory.MoneyDistribution
import Money.Theory.FinancialContract
\end{code}
}

\begin{haddock}
\begin{code}
{- $intro

Here are some models for different payment execution environment.

-}
\end{code}
\end{haddock}

\paragraph{Nondeterministic Sequential Execution Environment}

First we define a model for nondeterministic sequential payment execution environment, which includes a set of all
financial contracts and a step through function:

\begin{code}
-- | Nondeterministic sequential payment execution environment.
class ( MoneyDistribution md
      , FinancialContract fc md
      , Monad env
      ) => NondetSeqPaymentExecEnv env md fc | env -> md, env -> fc where
    -- | Monadically delete a financial contract from the execution environment.
    fcMDelete :: fc -> env ()

    -- | Monadically select one financial contract from the execution environment.
    fcMSelect :: ( ctx ~ MD_CTX md
                 , Timestamp t
                 )
              => t -> env (md, ctx, fc)

    -- | Step through the execution environment.
    penvStepThrough :: ( ctx ~ MD_CTX md
                      , Timestamp t
                      )
                  => t -> env (md, ctx)
    -- Default implementation for the step through function.
    penvStepThrough t = do
        (md, ctx, fc) <- fcMSelect t
        if fcPred fc (md, ctx) t
            then do
                fcMDelete fc
                -- (<>) operator is the binary operator for monoidal types.
                return ((md, ctx) <> fcExec fc (md, ctx) t)
            else return (md, ctx)
\end{code}

We do not assume that \textit{fcMSelect} yields a predicate that evalutes to true; since it could be an input from the
external world. This won't work with deterministic financial contract set.

The environment is a Monad, where diferent side effects for \textit{fcMSelect} can be encoded. For more generalized
interface to computation, arrows could be used instead \cite{hughes2000generalising}.

\paragraph{Parallel Execution}

When the executions of payment primitives can be in parallel, the shared resource problem of updating money
distribution, context and financial contract set arrises in data storage.

To model the parallel execution, ones must first study the concurrency control of the data storage system used
\cite{bernstein1981concurrency}; while formalism of parallel execution can be best done using Petri Nets
(\cite{petri1962kommunikation}, \cite{reisig2012petri})\footnote{Petri Nets World,
https://www.informatik.uni-hamburg.de/TGI/PetriNets/index.php}.

But a model in Haskell will not be provided for now.

\paragraph{Deterministic Execution}

To make the execution environment deterministic, stronger ordering conditions must be provided to the financial contract
type:

\begin{code}
-- | Financial contract that can be totally ordered.
class ( MoneyDistribution md
      , FinancialContract tofc md
      , Ord tofc)
      => TotallyOrderedFinancialContract tofc md

-- | A partially ordered data type (incomplete definition).
class Poset a

-- | Financial contract that can be partially ordered.
class ( MoneyDistribution md
      , FinancialContract tofc md
      , Poset tofc)
      => PartiallyOrderedFinancialContract tofc md
\end{code}

Total ordered financial contract could be used to model deterministic sequential execution environment:

\begin{code}
-- | Deterministic sequential payment execution environment.
class ( MoneyDistribution md
      , TotallyOrderedFinancialContract tofc md
      ) => DetSeqPaymentExecEnv env md tofc | env -> md, env -> tofc where
    -- | Insert a financial contract to the execution environment.
    fcInsert :: fc -> env -> env

    -- | Delete a financial contract from the execution environment.
    fcDelete :: fc -> env -> env

    -- | Deterministically get the next financial contract
    --   executable at a specific time.
    fcNext :: ( ctx ~ MD_CTX md
              , Timestamp t
              )
           => env -> (md, ctx, tofc, t)

    -- | Update execution environment with new money distribution and context.
    penvUpdate :: ctx ~ MD_CTX md
               => env -> (md, ctx) -> env

    -- | Deterministically step through the execution environment
    penvDetStepThrough :: ( ctx ~ MD_CTX md
                          , Timestamp t
                          )
                       => env -> (env, t)
    -- Default implementation for the step through function.
    penvDetStepThrough env = let
        (md, ctx, fc, t) = fcNext env
        -- assert: fcPred fc (md, ctx) t
        in (penvUpdate
            (fcDelete fc env)
            ((md, ctx) <> fcExec fc (md, ctx) t)
           , t)
\end{code}

The environment is no longer monad, and it is equivalent to say it is now fully deterministic. Instead the monadic
interations with external world should use \textit{fcInsert} for adding new financial contracts to the environment.

A weaker condition, namely a poset (partially ordered) of financial contracts, may enable deterministic parallel
executions of payments. Its model in Haskell will also not be provided for now.
