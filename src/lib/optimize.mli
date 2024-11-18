(* Machine Learning optimizer used to actually do backpropogation. It is a good way to test the automatic differentiation library*)
module Optimize : sig
  type t
  type f
  type optimizer = { init : t; step : t -> t; stop_condition : t -> bool }

  val gradient_descent : f -> optimizer -> t
  (** Performs gradient descent optimization on the function. *)

  val newton_raphson : f -> optimizer -> t
  (** Performs Newton-Raphson optimization using the Hessian. *)
end

module Visualize : sig
  val export_graph : filename:string -> unit
  (** Exports the current computational graph to a file. *)

  val show_graph : unit -> unit
  (** Displays the computational graph using an internal viewer. *)
end
