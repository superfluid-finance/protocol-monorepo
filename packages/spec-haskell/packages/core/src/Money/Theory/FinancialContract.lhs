\ignore{
\begin{code}
{-# LANGUAGE TypeFamilies #-}
module Money.Theory.FinancialContract where
\end{code}
}

\begin{code}
class FinancialContract fc where
  -- | Predicate of the execution condition of a financial contract.
  fcPred :: fc -> (md, ctx) -> Bool
  -- | Execute the payment primitives encoded in the financial contract.
  fcExec :: fc -> (md, ctx) -> (md, ctx)
\end{code}
