module type Tensor = sig
  type t
  val shape : t -> int list
  val zeros : int list -> t
  val ones : int list -> t
  val random : ?seed:int -> int list -> t
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
