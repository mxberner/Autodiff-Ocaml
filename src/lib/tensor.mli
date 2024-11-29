(* The main data type we'll run operations on *)
module type T = sig
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

  val add : t -> t -> t
  (** Element-wise addition of two values. Raises DimensionMismatch if shapes are incompatible. *)

  val sub : t -> t -> t
  (** Element-wise subtraction of two values. Raises DimensionMismatch if shapes are incompatible. *)

  val mul : t -> t -> t
  (** Element-wise multiplication of two values. Raises DimensionMismatch if shapes are incompatible. *)

  val div : t -> float -> t
  (** Element-wise division of two values. Raises DivisionByZero*)

  val dot : t -> t -> t
  (** Dot product of two values. For matrices, this represents matrix multiplication. *)

  val pow : t -> float -> t
  (** Raises each element of the value to the specified power. *)

  val log : t -> t
  (** Applies the natural logarithm element-wise. *)

  val exp : t -> t
  (** Applies the exponential function element-wise. *)

  val sin : t -> t
  (** Applies the sine function element-wise. *)

  val cos : t -> t
  (** Applies the cosine function element-wise. *)

  val tan : t -> t
  (** Applies the tangent function element-wise. *)

  val reshape : t -> int list -> t
  (** Reshapes the value to the specified dimensions. *)

  val transpose : t -> t
  (** Transposes the value (only applicable for matrices). *)

  val negate : t -> t
  (** Negates each element of the value. *)

  val flatten : t -> t
  (** Flattens the value into a one-dimensional array. *)

  val sum : t -> t
  (** Sums all elements of the value, returning a scalar value. *)

  (* Operator overloading for custom operations on matrices *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> float -> t
end

module Tensor : T
