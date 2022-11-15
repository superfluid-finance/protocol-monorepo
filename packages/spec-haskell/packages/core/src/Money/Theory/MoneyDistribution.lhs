> -- -*- fill-column: 70; -*-

\ignore{
\begin{code}
{-# LANGUAGE TypeFamilyDependencies #-}
module Money.Theory.MoneyDistribution
    -- $intro
    --
    ( MonetaryValue
    , Timestamp
    , SharedContext
    , Bearer
    , MonetaryUnit
    , MoneyDistribution (..)
    ) where

import Data.Default (Default)
import Data.Kind (Type)
import Data.Coerce ( Coercible )
\end{code}
}

\begin{haddock}
\begin{code}
{- $intro

As the innermost layer of a modern payment system, money distribution
models how monetary value is distributed amongst bearers.

-}
\end{code}
\end{haddock}

\begin{code}
class (Integral ν, Default ν) => MonetaryValue ν
\end{code}

\begin{code}
class (Integral t, Default t) => Timestamp t
\end{code}

\begin{code}
class Monoid ctx => SharedContext ctx
\end{code}

\begin{code}
class Bearer brr
\end{code}

\begin{code}
class Eq u => MonetaryUnit u
\end{code}

\begin{code}
-- | Money distribution functions and indexed types.
class ( MonetaryValue (MD_MVAL md)
      , Timestamp (MD_TS md)
      -- t & mval should have the same representational type
      , Coercible (MD_TS md) (MD_MVAL md)
      , MonetaryUnit (MD_MU md)
      , Bearer (MD_BRR md)
      , SharedContext (MD_CTX md)
      , Monoid md
      ) => MoneyDistribution md where
    -- | Set of bearers.
    bearers :: ( brr ~ MD_BRR md
               )
            => md -> [brr]

    -- | Set of monetary units.
    monetaryUnits :: mu ~ MD_MU md
                  => md -> [mu]

    -- | Money distribution β function.
    bearerOf :: ( mu  ~ MD_MU md
              , brr ~ MD_BRR md
              )
             => md -> mu -> brr

    -- | Money distribution ν function.
    monetaryValueOf :: ( mv  ~ MD_MVAL md
                     , mu  ~ MD_MU md
                     , ctx ~ MD_CTX md
                     , t   ~ MD_TS md
                     )
                    => md -> mu -> ctx -> t -> mv

    type family MD_MVAL md = (mval :: Type) | mval -> md
    type family MD_TS   md = (t    :: Type) | t    -> md
    type family MD_MU   md = (mu   :: Type) | mu   -> md
    type family MD_BRR  md = (brr  :: Type) | brr  -> md
    type family MD_CTX  md = (ctx  :: Type) | ctx  -> md
\end{code}
