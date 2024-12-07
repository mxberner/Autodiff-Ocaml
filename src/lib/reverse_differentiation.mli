
type f

type t

val gradient : f -> t -> t
(** Computes the gradient of the scalar-valued function at the given input. *)

val jacobian : f -> t -> t
(** Computes the Jacobian matrix of the function at the given input. *)

val hessian : f -> t -> t
(** Computes the Hessian matrix (second-order derivatives) of the function at the given input. *)

val element_wise_gradient : f -> t -> t
(** Computes the gradient of each element in the output with respect to the input. *)

val checkpoint : f -> f
(** Allows for the saving and restoring of computation graphs to manage memory usage. *)
