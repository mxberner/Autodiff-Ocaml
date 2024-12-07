type v = { id : int; value : float; local_gradients : (v * float) list }

module VariableHashtbl : sig
  type 'a t
end

val make : float -> v
(* Create the variable from a float *)

val neg : v -> v
(* Negate the variable *)

val inv : v -> v
(* Invert the variable *)

val add : v -> v -> v
(* Create the variable that results from adding two variables *)

val sub : v -> v -> v
(* Create the variable that results from subtracting two variables *)

val mul : v -> v -> v
(* Create the variable that results from multiplying two variables *)

val div : v -> v -> v
(* Create the variable that results from dividing two variables *)

val sin : v -> v
(* Sine of the variable *)

val cos : v -> v
(* Cosine of the variable *)

val tan : v -> v
(* Tangent of the variable *)

val gradients : v -> float VariableHashtbl.t
(* Compute the *local gradients* of all the variable *)

val find : float VariableHashtbl.t -> v -> float
(* Find the local gradient of a variable *)

val ( + ) : v -> v -> v
val ( - ) : v -> v -> v
val ( * ) : v -> v -> v
val ( / ) : v -> v -> v
