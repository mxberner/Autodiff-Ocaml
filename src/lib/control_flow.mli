module type CF = sig
  type t
  type f = t -> int -> t * int
  (* Function takes a variable and id_counter, returns a variable and updated id_counter *)

  val if_then_else : (t -> bool) -> f -> f -> f
  (** Represents an if-then-else control flow construct. *)

  val while_loop : (t -> bool) -> f -> t -> int -> (t * int)
  (** Represents a while loop construct. *)

  val for_loop : t -> t -> f -> int -> t * int
  (** Represents a for loop construct. *)
end

module ControlFlow: CF