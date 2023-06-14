> -- -*- fill-column: 70; -*-

\ignore{
\begin{code}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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

Here are some models for the different payment execution environments.

-}
\end{code}
\end{haddock}

\paragraph{Non-deterministic Sequential Execution Environment}

First, we define a model for a non-deterministic sequential payment
execution environment, which includes a set of all financial contracts
and a step-through function:

\begin{code}
-- | Non-deterministic sequential payment execution environment.
class ( MoneyDistribution md
      , FinancialContract fc md
      , Monad env
      ) => NondetSeqPaymentExecEnv env md fc | env -> md, env -> fc where
    -- | Monadically update a financial contract in the execution
    -- environment.
    fcMUpdate :: fc -> env ()

    -- | Monadically select one financial contract from the execution
    -- environment.
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
                let ((md', ctx'), fc') = fcExec fc (md, ctx) t
                fcMUpdate fc'
                -- (<>) operator is the binary operator for monoidal types.
                return ((md, ctx) <> (md', ctx'))
            else return (md, ctx)
\end{code}

We do not assume that \textit{fcMSelect} yields a predicate that
evaluates to true; since it could be an input from the external
world. This won't work with any deterministic financial contract set.

The environment is a Monad, where different side effects for
\textit{fcMSelect} can be encoded. However, arrows could be used
instead for a more generalized interface to
computation\cite{hughes2000generalising}.

\paragraph{Parallel Execution}

When the executions of payment primitives can be parallel,
resource-sharing problems arise in their data storage when updating
money distribution, context, and financial contract sets.

To model the parallel execution, ones must first study the concurrent
control of the data storage system used
(\cite{bernstein1981concurrency}), while formalism of parallel
execution can be best done using Petri Nets
(\cite{petri1962kommunikation}, \cite{reisig2012petri})\footnote{Petri
Nets World,
https://www.informatik.uni-hamburg.de/TGI/PetriNets/index.php}.

A model in Haskell will not be provided for now since it is out of the
scope of the paper.

\paragraph{Deterministic Execution}

To make the execution environment deterministic, stronger ordering
conditions must be provided to the financial contract type:

\begin{code}
-- | Financial contract that can be totally ordered.
class ( MoneyDistribution md
      , FinancialContract tofc md
      , Ord tofc)
      => TotallyOrderedFinancialContract tofc md

-- | A partially ordered data type (incomplete definition).
class Poset a
-- omitting detailed interface of it.

-- | Financial contract that can be partially ordered.
class ( MoneyDistribution md
      , FinancialContract tofc md
      , Poset tofc)
      => PartiallyOrderedFinancialContract tofc md
\end{code}

Total ordered financial contract could be used to model deterministic
sequential execution environment:

\begin{code}
-- | Deterministic sequential payment execution environment.
class ( MoneyDistribution md
      , TotallyOrderedFinancialContract tofc md
      ) => DetSeqPaymentExecEnv env md tofc | env -> md, env -> tofc where
    -- | Update a financial contract in the execution environment.
    --
    --   Note:
    --
    --     * In order to keep well-ordering properties, the complexity
    --       of this function can be at least as bad as updating a
    --       sorted data structure $O(log(n))$.
    fcUpdate :: fc -> env -> env

    -- | Deterministically get the next financial contract
    --   executable at a specific time.
    fcNext :: ( ctx ~ MD_CTX md
              , Timestamp t
              )
           => env -> (md, ctx, tofc, t)

    -- | Update execution environment with new money distribution and
    -- context.
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
        ((md', ctx'), fc') = fcExec fc (md, ctx) t
        -- assert: fcPred fc (md, ctx) t
        in (penvUpdate
            (fcUpdate fc' env)
            ((md, ctx) <> (md', ctx'))
           , t)
\end{code}

The environment is no longer monadic; that is to say, it is now fully
deterministic. Instead, the monadic interactions with the external
world should use \textit{fcInsert} for adding new financial contracts
to the environment.

A weaker condition, namely a poset (partially ordered) of financial
contracts, may enable deterministic parallel executions of
payments. However, model in Haskell will also not be provided for now.
