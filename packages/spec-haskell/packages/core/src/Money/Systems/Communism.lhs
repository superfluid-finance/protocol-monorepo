Communism comes in various forms, and all have its own money distribution "system".

\ignore{
\begin{code}
module Money.Systems.Communism where

import Money.Concepts.Distribution
\end{code}
}

\lhsparagraph{Utopian Communism}

In its utopian form, there is no more scarcity, but the number of its members are finite. Hence the any meaningful money
unit would also come with unlimited liquidity.

\begin{code}
data Infinite a = Infinite | Only a

data UtopianCommunism

instance LiquidityDistribution UtopianCommunism where
    bearers = undefined
    liquidityOf = undefined
\end{code}

\lhsparagraph{Dystopian Communism}

In its more likely form, there is simply nothing, and everyone owns nothing.

\begin{code}
data DystopianCommunism

instance LiquidityDistribution DystopianCommunism where
    bearers = undefined
    liquidityOf = undefined
\end{code}

\lhsparagraph{Autocratic Communism}

In a more realistic form, the one that determines the liquidity amount of each money unit is at the mercy of the side
effect of the monadic autocrat.

\begin{code}
data AutocraticCommunism

instance LiquidityDistribution AutocraticCommunism where
    bearers = undefined
    liquidityOf = undefined
\end{code}

\lhsparagraph{Communism Bad}

In colcusion, as it has been demonstrated, all forms of communism are not workable as it's either neither functional nor
pure money redistribution mechanism.
