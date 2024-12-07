open Variable
open Tensor

val gradient : (v -> v) -> t -> t
(** Computes the gradient of the scalar-valued function at the given input. *)

val jacobian : t -> t -> t
(** Computes the Jacobian matrix of the function at the given input. *)

val hessian : t -> t -> t
(** Computes the Hessian matrix (second-order derivatives) of the function at the given input. *)

val element_wise_gradient : t -> t
(** Computes the gradient of each element in the output with respect to the input. *)

val checkpoint : t -> unit
(** Allows for the saving and restoring of computation graphs to manage memory usage. *)
