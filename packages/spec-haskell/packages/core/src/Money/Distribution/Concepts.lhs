An attempt of creating a unifying theory for money and its distribution was made by Buldas, Saarepera et al.\cite{buldas2021unifying}

\begin{displayquote}
A useful observation about existing money schemes is that they all have some kind of monetary units that
are physical or digital representations of money. Examples are bills, coins, bank accounts, Bitcoin UTXOs,
etc.
\end{displayquote}

\ignore{
\begin{code}
module Money.Distribution.Concepts where
\end{code}
}

\begin{code}
-- | Liquidity
class Integral lq => Liquidity lq
\end{code}

\begin{code}
-- | Money Distribution
class MoneyDistribution md
\end{code}

(WIP)
