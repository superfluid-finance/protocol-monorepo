\ignore{
\begin{code}
{-# LANGUAGE TypeFamilies #-}
module Money.Theory.MoneyDistribution where

import Data.Default (Default)
import Data.Kind (Type)
\end{code}
}

\begin{code}
class (Integral mv, Default mv) => MonetaryValue mv
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
      ) => MoneyDistribution md where
    type MD_MV  md :: Type
    type MD_MU  md :: Type
    type MD_BRR md :: Type
    type MD_CTX md :: Type

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
\end{code}
