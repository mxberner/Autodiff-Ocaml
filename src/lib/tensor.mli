(* The main data type we'll run operations on *)
module type Tensor = sig
    type t
    (** The type representing numerical values, which could be scalars, vectors, or matrices. *)
  
    val shape : t -> int list
    (** Returns the dimensions of the value as a list of integers. *)
  
    val zeros : int list -> t
    (** Creates a n x n tensor value filled with zeros, given the specified dimensions. *)
  
    val ones : int list -> t
    (** Creates a n x n tensor value filled with ones, given the specified dimensions. *)
  
    val random : ?seed:int -> int list -> t
    (** Generates a value with random entries, given the specified dimensions and an optional seed. *)
  end