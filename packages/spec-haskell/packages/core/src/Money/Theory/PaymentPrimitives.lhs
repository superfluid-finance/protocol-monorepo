> -- -*- fill-column: 70; -*-

\ignore{
\begin{code}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
module Money.Theory.PaymentPrimitives
    -- $intro
    --
    ( MoneyDistributionModel
    , 𝓜
    , 𝓜' (..)
    , sem𝓜
    ) where

import Data.Coerce ( coerce )

import Money.Theory.MoneyDistribution
\end{code}
}

\begin{haddock}
\begin{code}
{- $intro

Here is the denotational semantics of payment primitives of modern
payment system.

-}
\end{code}
\end{haddock}

\begin{code}
-- | Type synonym for ⟦𝓜⟧.
type MoneyDistributionModel' md = forall ν t u.
    ( MoneyDistribution md
    , ν ~ MD_MVAL md
    , t ~ MD_TS md
    , u ~ MD_MU md
    ) => u -> t -> ν

-- | ⟦𝓜⟧ - methematical model of meaning in money distribution.
data MoneyDistributionModel md = MkMoneyDistributionModel
    (MoneyDistributionModel' md)
\end{code}

\begin{code}
-- | Semigroup class instance ⟦𝓜⟧.
instance ( MoneyDistribution md
         ) => Semigroup (MoneyDistributionModel md) where
    -- ⊕: monoid binary operator
    (MkMoneyDistributionModel ma) <> (MkMoneyDistributionModel mb) =
        MkMoneyDistributionModel (\u -> \t -> ma u t + mb u t)

-- | Monoid class instance ⟦𝓜⟧.
instance ( MoneyDistribution md
         ) => Monoid (MoneyDistributionModel md) where
    -- ∅: monoid empty set
    mempty = MkMoneyDistributionModel (\_ -> \_ -> 0)
\end{code}

\begin{code}
-- | 𝓜' - syntactic category.
data 𝓜' ν t u =
    Transfer u u ν |
    Flow u u ν t

-- | Type synonym for 𝓜' using type family.
type 𝓜 md = forall u t ν.
    ( MoneyDistribution md
    , ν ~ MD_MVAL md
    , t ~ MD_TS md
    , u ~ MD_MU md
    ) => 𝓜' ν t u

-- | ⟦.⟧ - semantic function of 𝓜.
sem :: MoneyDistribution md
    => 𝓜 md -> MoneyDistributionModel' md
sem (Transfer from to amount) = \u -> \_ -> if
    | from == u -> -amount
    | to   == u ->  amount
    | otherwise ->  0
sem (Flow from to r t') = \u -> \t -> if
    | from == u -> -r * coerce (t - t')
    | to   == u ->  r * coerce (t - t')
    | otherwise ->  0
-- GHC 9.4.2 bug re non-exhaustive pattern matching?
sem _ = error "huh?"

-- | ⟦.⟧ - semantic function of 𝓜.
sem𝓜 :: MoneyDistribution md
     => 𝓜 md -> MoneyDistributionModel md
sem𝓜 s = MkMoneyDistributionModel (sem s)
\end{code}
