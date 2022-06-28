An attempt of creating a unifying theory for money and its distribution was made by Buldas, Saarepera et al.\cite{buldas2021unifying}

\begin{displayquote}
A useful observation about existing money schemes is that they all have some kind of monetary units that are physical or
digital representations of money. Examples are bills, coins, bank accounts, Bitcoin UTXOs, etc.
\end{displayquote}

\ignore{
\begin{code}
module Money.Concepts.Distribution where

import Data.Default ( Default )
\end{code}
}

\begin{code}
-- | Liquidity Class
--
-- Naming conventions:
--
--   * Type name: lq
class (Default lq, Integral lq, Show lq) => Liquidity lq
\end{code}

\begin{code}
class Context ctx
\end{code}

\begin{code}
class Bearer brr
\end{code}

\begin{code}
-- | Liquidity Distribution
class LiquidityDistribution ld where
    bearers :: Bearer brr => ld -> [brr]

    liquidityOf :: (Context ctx, Bearer brr, Liquidity liq) => ld -> (brr, ctx) -> liq
\end{code}

(WIP)
