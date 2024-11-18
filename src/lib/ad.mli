module type Tensor = sig
  type t

  val shape : t -> int list
  val zeros : int list -> t
  val ones : int list -> t
  val random : ?seed:int -> int list -> t
end

module type Errors = sig
  exception DimensionMismatch of string
  exception InvalidArgument of string
  exception DivisionByZero
end

module type Op = sig
  include Tensor
  include Errors

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
  val ( / ) : t -> t -> t
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

module type Differentiation = sig
  include Op
  include Function with type t := t
  include ControlFlow with type t := t

  val gradient : f -> t -> t
  val jacobian : f -> t -> t
  val hessian : f -> t -> t
  val element_wise_gradient : f -> t -> t
  val checkpoint : f -> f
end


module type ReverseDifferentiation = sig
  include Op
  include Function with type t := t
  include ControlFlow with type t := t

  val gradient : f -> t -> t
  val jacobian : f -> t -> t
  val hessian : f -> t -> t
  val element_wise_gradient : f -> t -> t
  val checkpoint : f -> f
end

(** The main module functor that, given a Value module, produces a Differentiation module with the associated types. *)
module type AD = functor (V : Tensor) -> sig
  include Differentiation with type t = V.t
  include ReverseDifferentiation with type t = V.t
end
