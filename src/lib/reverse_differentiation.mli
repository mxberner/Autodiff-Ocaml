open Variable

val jacobian : v -> v -> v
(** Computes the Jacobian matrix of the function at the given input. *)

val hessian : v -> v -> v
(** Computes the Hessian matrix (second-order derivatives) of the function at the given input. *)

val element_wise_gradient : v -> v
(** Computes the gradient of each element in the output with respect to the input. *)

val checkpoint : v -> unit
(** Allows for the saving and restoring of computation graphs to manage memory usage. *)
