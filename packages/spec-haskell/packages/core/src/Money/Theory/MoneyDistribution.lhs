\ignore{
\begin{code}
{-# LANGUAGE TypeFamilyDependencies #-}
module Money.Theory.MoneyDistribution where

import Data.Default (Default)
import Data.Kind (Type)
import Data.Coerce ( Coercible )
\end{code}
}

\begin{code}
class (Integral v, Default v) => MonetaryValue v
\end{code}

\begin{code}
class (Default t, Integral t) => Timestamp t
\end{code}

\begin{code}
class SharedContext ctx
\end{code}

\begin{code}
class Bearer brr
\end{code}

\begin{code}
class Eq u => MonetaryUnit u
\end{code}

\begin{code}
class ( MonetaryValue (MD_MVAL md)
      , Timestamp (MD_TS md)
      , Coercible (MD_TS md) (MD_MVAL md)
      , MonetaryUnit (MD_MU md)
      , Bearer (MD_BRR md)
      , SharedContext (MD_CTX md)
      , Monoid md, Monoid (MD_CTX md)
      ) => MoneyDistribution md where
    monetaryUnits :: mu ~ MD_MU md
                  => md -> [mu]

    monetaryValue :: ( mv  ~ MD_MVAL md
                     , mu  ~ MD_MU md
                     , ctx ~ MD_CTX md
                     )
                  => md -> (mu, ctx) -> mv

    bearer :: ( mu  ~ MD_MU md
              , brr ~ MD_BRR md
              ) => md -> mu -> brr

    type family MD_MVAL md = (mval :: Type) | mval -> md
    type family MD_TS   md = (t    :: Type) | t    -> md
    type family MD_MU   md = (mu   :: Type) | mu   -> md
    type family MD_BRR  md = (brr  :: Type) | brr  -> md
    type family MD_CTX  md = (ctx  :: Type) | ctx  -> md
\end{code}
