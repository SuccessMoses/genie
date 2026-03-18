inductive BExpr where
| True
| False
| If (ante : BExpr) (e1 : BExpr) (e2 : BExpr)

inductive BExpr.Semantics : BExpr → BExpr → Prop where
| IfTrue (t₂ t₃ : BExpr) : Semantics (If True t₂ t₃) t₂
| IfFalse (t₂ t₃ : BExpr) : Semantics (If False t₂ t₃) t₃
| IfCongr (t₁ t₁' t₂ t₃ : BExpr) :
    Semantics t₁ t₁' → Semantics (If t₁ t₂ t₃) (If t₁' t₂ t₃)

-- write using '\' + '~>'
notation t₁ " ⤳ " t₂ => BExpr.Semantics t₁ t₂

namespace BExpr

abbrev tru := BExpr.True

abbrev fls := BExpr.False

notation "if_ " t₁ "then " t₂ "else " t₃ => BExpr.If t₁ t₂ t₃

abbrev s := if_ tru then fls else fls

abbrev t := if_ s then tru else tru

abbrev u := if_ fls then tru else tru

example : (if_ t then fls else fls) ⤳ (if_ u then fls else fls) := by
  refine Semantics.IfCongr _ _ _ _ ?_
  refine Semantics.IfCongr _ _ _ _ ?_
  refine Semantics.IfTrue _ _
