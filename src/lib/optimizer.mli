(* Machine Learning optimizer used to actually do backpropogation. It is a good way to test the automatic differentiation library*)
open Tensor

type f = t -> t
type optimizer = { init : t; step : t -> t; stop_condition : t -> bool }

val gradient_descent : f -> optimizer -> t
(** Performs gradient descent optimization on the function. *)
