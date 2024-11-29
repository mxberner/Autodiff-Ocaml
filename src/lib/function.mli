module type Tensor = sig
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

module type Function = sig
  include Tensor

  type f = t -> t
  (** A function that maps values of type t to values of type t. *)

  val eval : f -> t -> t
  (** Evaluates the function at the given input value. *)

  val compose : f -> f -> f
  (** Composes two functions. *)

  val map : (t -> t) -> f
  (** Lifts a value-to-value function to operate over the function type f. *)
end
