An attempt of creating a unifying theory for money and its distribution was made by Buldas, Saarepera et al.\cite{buldas2021unifying}

\begin{displayquote}
A useful observation about existing money schemes is that they all have some kind of monetary units that are physical or
digital representations of money. Examples are bills, coins, bank accounts, Bitcoin UTXOs, etc.
\end{displayquote}

\ignore{
\begin{code}
module Money.Concepts.Distribution where

import Data.Default (Default)
\end{code}
}

\begin{code}
-- | Value Class
--
-- Naming conventions:
--
--   * Type name: v
class (Integral v, Default v) => Value v
\end{code}

\begin{code}
class Context ctx
\end{code}

\begin{code}
class Bearer brr
\end{code}

\begin{code}
class MonetaryUnit mu
\end{code}

\begin{code}
-- | Value Distribution
class Distribution ld where
    -- type MONEY_UNIT ld :: Type

    moneyUnits :: MonetaryUnit mu => ld -> [mu]

    bearerOf :: (MonetaryUnit mu, Bearer brr) => ld -> mu -> brr

    valueOf :: (Context ctx, MonetaryUnit mu, Value v) => ld -> (mu, ctx) -> v
\end{code}

(WIP)
