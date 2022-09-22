\ignore{
\begin{code}
{-# LANGUAGE TypeFamilies #-}
module Money.Theory.Distribution where

import Data.Default (Default)
-- import Data.Kind (Type)
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
class MoneyDistribution d where
    -- type DistributionValueType        d :: Type
    -- type DistributionMonetaryUnitType d :: Type
    -- type DistributionBearerType       d :: Type
    -- type DistributionContextType      d :: Type

    monetaryUnits :: MonetaryUnit mu => d -> [mu]

    valueOf :: (Context ctx, MonetaryUnit mu, Value v) => d -> (mu, ctx) -> v

    bearerOf :: (MonetaryUnit mu, Bearer brr) => d -> mu -> brr
\end{code}
