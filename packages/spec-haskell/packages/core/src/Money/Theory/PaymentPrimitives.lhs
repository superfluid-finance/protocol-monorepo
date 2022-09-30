\ignore{
\begin{code}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
module Money.Theory.PaymentPrimitives
    -- $intro
    --
    ( PaymentPrimitive (..)
    ) where

import Data.Coerce ( coerce )

import Money.Theory.MoneyDistribution
\end{code}
}

\begin{code}
data MoneyDistributionModel md = forall u t v.
    ( t ~ MD_TS md
    , u ~ MD_MU md
    , v ~ MD_MVAL md
    ) => MkMoneyDistributionModel (u -> t -> v)

instance ( MoneyDistribution md
         ) => Semigroup (MoneyDistributionModel md) where
    -- monoid binary operator
    (MkMoneyDistributionModel ma) <> (MkMoneyDistributionModel mb) =
        MkMoneyDistributionModel (\u -> \t -> ma u t + mb u t)

instance ( MoneyDistribution md
         ) => Monoid (MoneyDistributionModel md) where
    -- monoid empty set
    mempty = MkMoneyDistributionModel
        (\_ -> \_ -> 0)


class ( MoneyDistribution md
      ) => PaymentPrimitive prim md | prim -> md where
    runPrim :: prim -> MoneyDistributionModel md

data Transfer md = forall u v.
    ( u ~ MD_MU md
    , v ~ MD_MVAL md
    ) => Transfer u u v
instance ( MoneyDistribution md
         ) => PaymentPrimitive (Transfer md) md where
    runPrim (Transfer from to amount) = MkMoneyDistributionModel (
        \u -> \_ -> if
            | from == u -> -amount
            | to   == u -> amount
            | otherwise -> 0
        )

data Flow md = forall u t v.
    ( u ~ MD_MU md
    , v ~ MD_MVAL md
    , t ~ MD_TS md
    ) => Flow u u v t
instance ( MoneyDistribution md
         ) => PaymentPrimitive (Flow md) md where
    runPrim (Flow from to r t') = MkMoneyDistributionModel (
        \u -> \t -> if
            | from == u -> -r * coerce (t - t')
            | to   == u ->  r * coerce (t - t')
            | otherwise -> 0
        )
\end{code}
