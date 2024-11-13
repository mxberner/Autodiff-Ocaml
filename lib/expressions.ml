
(*defines all types of expressions*)
type expr = 
  | Const of float     (**  [Const] represents constant [float]     *)
  | Var of string      (**  [Var] represents variable [string]      *)
  | Add of expr * expr (**  [Add] [expr A] [expr B] represents A+B  *)
  | Sub of expr * expr (**  [Sub] [expr A] [expr B] represents A-B  *)
  | Mul of expr * expr (**  [Mul] [expr A] [expr B] represents A*B  *)
  | Div of expr * expr (**  [Div] [expr A] [expr B] represents A/B  *)
  | Pow of expr * expr (**  [Pow] [expr A] [expr B] represents A^B  *)
  | Sin of expr        (**  [Sin] [expr A] represents sin(A)        *)
  | Cos of expr        (**  [Cos] [expr A] represents cos(A)        *)
  | Ln of expr         (**  [Ln] [expr A] represents ln(A)          *)

(*evaluates the expression*)
let rec eval expr var_value =
    match expr with
    | Const c -> c
    | Var _ -> var_value
    | Add (e1, e2) -> eval e1 var_value +. eval e2 var_value
    | Sub (e1, e2) -> eval e1 var_value -. eval e2 var_value
    | Mul (e1, e2) -> eval e1 var_value *. eval e2 var_value
    | Div (e1, e2) -> eval e1 var_value /. eval e2 var_value
    | Pow (e1, e2) -> eval e1 var_value ** eval e2 var_value
    | Sin e -> sin (eval e var_value)
    | Cos e -> cos (eval e var_value)
    | Ln e -> log (eval e var_value)
