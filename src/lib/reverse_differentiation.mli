(* module type Tensor = sig
  type t

  val shape : t -> int list
  val zeros : int list -> t
  val ones : int list -> t
  val random : ?seed:int -> int list -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> float -> t
  val dot : t -> t -> t
  val pow : t -> float -> t
  val log : t -> t
  val exp : t -> t
  val sin : t -> t
  val cos : t -> t
  val tan : t -> t
  val reshape : t -> int list -> t
  val transpose : t -> t
  val negate : t -> t
  val flatten : t -> t
  val sum : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> float -> t
end

module type Errors = sig
  exception DimensionMismatch of string
  exception InvalidArgument of string
  exception DivisionByZero
end

module type Function = sig
  include Tensor

  type f = t -> t

  val eval : f -> t -> t
  val compose : f -> f -> f
  val map : (t -> t) -> f
end

module type ControlFlow = sig
  type t
  type f = t -> t

  val if_then_else : (t -> bool) -> f -> f -> f
  val while_loop : (t -> bool) -> f -> t -> t
  val for_loop : t -> t -> f -> t
end

(* We implement a reverse differentiation module because it is more efficienct for certain functions *)
module type RD = sig
  include Tensor
  include Function with type t := t
  include ControlFlow with type t := t

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
end

module ReverseDifferentiation: RD *)
