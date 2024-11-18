module type ControlFlow = sig
  type t
  type f = t -> t

  val if_then_else : (t -> bool) -> f -> f -> f
  (** Represents an if-then-else control flow construct. *)

  val while_loop : (t -> bool) -> f -> t -> t
  (** Represents a while loop construct. *)

  val for_loop : t -> t -> f -> t
  (** Represents a for loop construct. *)
end
