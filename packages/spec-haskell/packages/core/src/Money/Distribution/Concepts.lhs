An attempt of creating a unifying theory for money and its distribution was made by Buldas, Saarepera et al.\cite{buldas2021unifying}

\begin{displayquote}
A useful observation about existing money schemes is that they all have some kind of monetary units that are physical or
digital representations of money. Examples are bills, coins, bank accounts, Bitcoin UTXOs, etc.
\end{displayquote}

\ignore{
\begin{code}
module Money.Distribution.Concepts where

import Data.Default
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
-- | Timestamp Type Class
--
-- Naming conventions:
--  * Type name: ts
--  * SuperfluidTypes type indexer: SFT_TS
class (Default ts, Integral ts, Show ts) => Timestamp ts
\end{code}

\begin{code}
-- | Money Distribution
class MoneyDistribution md
\end{code}

(WIP)
