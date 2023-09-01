open import Relation.Binary
  using (TotalOrder)
open import Level

module SemanticMoney
  {ℓₜ ℓᵣ : Level}
  (Ord : TotalOrder ℓₜ ℓᵣ ℓᵣ)
  where

open import Relation.Binary.PropositionalEquality
  using (_≡_; refl)
open TotalOrder Ord
  using ()
  renaming (Carrier to 𝕋)
open import Function
  using (id; _∘_; _∘′_)
open import Data.Product
  using (_,_; _×_; proj₁; proj₂)
open import Data.Integer as Int
  using (ℤ)

private
  variable
    ℓ : Level
    A B C : Set ℓ

-- | Behavior in general.
β : Set ℓ -> Set (ℓₜ ⊔ ℓ)
β A = 𝕋 -> A

-- | Semantic function 'at'.
_at_ : β A -> 𝕋 -> A
b at t = b t

-- | Behavior of time.
time : β 𝕋
time = id

-- | Meaning of time.
⟦time⟧ : ∀ (t : 𝕋) -> time at t ≡ t
⟦time⟧ t = refl

lift₁ : (A -> B) -> β A -> β B
lift₁ = _∘′_ -- This is a non-dependently-typed composition for point-free style.

⟦lift₁⟧ : ∀ (t : 𝕋) -> ∀ (f : A -> B) -> ∀ (b₀ : β A)
       -> f (b₀ at t) ≡ (lift₁ f b₀) at t
⟦lift₁⟧ t f b₀ = refl

lift₂ : (A -> B -> C) -> β A -> β B -> β C
lift₂ f a b = λ t → f (a t) (b t) -- TODO: point-free style, how??

⟦lift₂⟧ : ∀ (t : 𝕋) -> ∀ (f : A -> B -> C) -> ∀ (b₀ : β A)  -> ∀ (b₁ : β B)
       -> f (b₀ at t) (b₁ at t) ≡ (lift₂ f b₀ b₁) at t
⟦lift₂⟧ t f b₀ b₁ = refl

-- | Value type alias. TODO use more general Ring instead.

V = ℤ

_+_ : β V -> β V -> β V
(x + y) t = x t Int.+ y t

-_ : β V -> β V
(- x) t = Int.- x t

<+> : β V × β V -> β V
<+> (b₀ , b₁) = b₀ + b₁ -- TODO use lift₂, also define the meaning of the product.

-- shift₀

-- flow₀

-- | Two-party payment function.
pay₂ : β V × β V -> β V -> β V × β V
-- pay₂ (from , to) op = (to , from)
pay₂ (from , to) op = from + op , to + (- op)

-- | Meaning of two-party payment.
⟦pay₂⟧ :
  ∀ (from to op : β V) ->
  ∀ (t : 𝕋) ->
  (from + to) at t ≡ (<+> (pay₂ (from , to) op)) at t
⟦pay₂⟧ = {!!} -- TODO so what it is?

-- | Property of two-party payment for the "from" party.
pay₂-prop-from-op :
  ∀ (from to op : β V) ->
  ∀ (t : 𝕋) ->
  (from + op) at t ≡ (proj₁ (pay₂ (from , to) op)) at t
pay₂-prop-from-op = λ from to op t → refl

-- | Property of two-party payment for the "to" party.
pay₂-prop-to-op :
  ∀ (from to op : β V) ->
  ∀ (t : 𝕋) ->
  (to + (- op)) at t ≡ (proj₂ (pay₂ (from , to) op)) at t
pay₂-to = λ from to op t → refl
