\ignore{
\begin{code}
{-# LANGUAGE TypeFamilyDependencies #-}
module Money.Theory.MoneyDistribution where

import Data.Default (Default)
import Data.Kind (Type)
\end{code}
}

\begin{code}
class (Integral mv, Default mv) => MonetaryValue mv
\end{code}

\begin{code}
class (Default ts, Integral ts) => Timestamp ts
\end{code}

\begin{code}
class SharedContext ctx
\end{code}

\begin{code}
class Bearer brr
\end{code}

\begin{code}
class MonetaryUnit mu
\end{code}

\begin{code}
class ( MonetaryValue (MD_MV md)
      , MonetaryUnit (MD_MU md)
      , Bearer (MD_BRR md)
      , SharedContext (MD_CTX md)
      , Monoid md, Monoid (MD_CTX md)
      ) => MoneyDistribution md where
    monetaryUnits :: mu ~ MD_MU md
                  => md -> [mu]

    monetaryValue :: ( mv  ~ MD_MV md
                     , mu  ~ MD_MU md
                     , ctx ~ MD_CTX md
                     )
                  => md -> (mu, ctx) -> mv

    bearer :: ( mu  ~ MD_MU md
              , brr ~ MD_BRR md
              ) => md -> mu -> brr

    type family MD_MV  md = (mv  :: Type) | mv  -> md
    type family MD_MU  md = (mu  :: Type) | mu  -> md
    type family MD_BRR md = (brr :: Type) | brr -> md
    type family MD_CTX md = (ctx :: Type) | ctx -> md
\end{code}
