open Tensor

type v = { id : int; data : t; local_gradients : (v * (v -> v)) list }

module VariableHashtbl : sig
  type 'a t
end

val make : ?local_gradients:(v * (v -> v)) list -> t -> v
(* Create the variable from a Tensor *)

val zero : unit -> v
(* Create a variable with 0 value*)

val one : unit -> v
(* Create a variable with 0 value*)

val random : ?seed:int -> dims -> v
(* Create a variable with random value*)

val add : v -> v -> v
(* Create the variable that results from adding two variables *)

val mul : v -> v -> v
(* Create the variable that results from multiplying two variables *)

val div : v -> v -> v
(* Create the variable that results from multiplying two variables *)

val neg : v -> v
(* Negate the variable *)

val inv : v -> v
(* Inverse of the variable *)

val sub : v -> v -> v
(* Create the variable that results from subtracting two variables *)

val pow : v -> float -> v
(* Power of the variable *)

val compare : v -> v -> int
(* Compare two variable float values *)

val equal : v -> v -> bool
(* Check if two variable float values are equal*)

val sin : v -> v
(* Sine of the variable *)

val cos : v -> v
(* Cosine of the variable *)

val tan : v -> v
(* Tangent of the variable *)

val exp : v -> v
(* exp of the variable *)

val log : v -> v
(* log of the variable *)

val gradients : v -> v VariableHashtbl.t
(* Compute the *local gradients* of all the variable *)

val find : v VariableHashtbl.t -> v -> v
(* Find the local gradient of a variable *)

val print : v -> unit

(* Operator overloading *)
val ( + ) : v -> v -> v
val ( - ) : v -> v -> v
val ( * ) : v -> v -> v
val ( / ) : v -> v -> v
val ( ** ) : v -> float -> v
val ( = ) : v -> v -> bool

(* Tensor operations with gradient calculations *)

val get : v -> dims -> float

val sum : v -> v
(** [sum v] computes the sum of all elements in the tensor, returning a scalar value. *)

val dot : v -> v -> v
(** [dot v1 v2] computes the dot product of two vectors. *)

val matmul : v -> v -> v
(** [matmul v1 v2] performs matrix multiplication on two matrices.
        The number of columns in the first matrix must match the number of rows in the
        second matrix. *)

val map : (float -> float) -> v -> v
(** [matmul v1 v2] performs matrix multiplication on two matrices.
        The number of columns in the first matrix must match the number of rows in the
        second matrix. *)
