Communism comes in various forms, and all have its own money distribution "system".

\ignore{
\begin{code}
module Money.Systems.Communism where

import Money.Concepts.Distribution
\end{code}
}

\lhsparagraph{Utopian Communism}

In its utopian form, there is no more scarcity, but the number of its members are finite. Hence the any meaningful monetary
unit would also come with unlimited value.

\begin{code}
data Infinite a = Infinite | Only a

data UtopianCommunism

instance Distribution UtopianCommunism where
    monetaryUnits = undefined
    bearerOf = undefined
    valueOf = undefined
\end{code}

\lhsparagraph{Dystopian Communism}

In its more likely form, there is simply nothing, and everyone owns nothing.

\begin{code}
data DystopianCommunism

instance Distribution DystopianCommunism where
    monetaryUnits = undefined
    bearerOf = undefined
    valueOf = undefined
\end{code}

\lhsparagraph{Autocratic Communism}

In a more realistic form, the one that determines the value amount of each monetary unit is at the mercy of the side
effect of the monadic autocrat.

\begin{code}
data AutocraticCommunism

instance Distribution AutocraticCommunism where
    monetaryUnits = undefined
    bearerOf = undefined
    valueOf = undefined
\end{code}

\lhsparagraph{Communism Bad}

In colcusion, as it has been demonstrated, all forms of communism are not workable as it's either neither functional nor
pure money redistribution mechanism.
