module type OpType = sig
  type t

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

module type OpFunctor = functor (T : Tensor.Type) -> OpType with type t = T.t

module Op : OpFunctor
