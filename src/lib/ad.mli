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

(* Some common errors when operating with n-dimensional tensors *)
module type Errors = sig
  exception DimensionMismatch of string
  exception InvalidArgument of string
  exception DivisionByZero
end


module type Op = sig
  include Tensor

  include Errors

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
  
  val ( / ) : t -> t -> t
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

(* A way to represent the function control flow that we are trying to differentiate *)
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


module type Differentiation = sig
  include Op
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

(* We implement a reverse differentiation module because it is more efficienct for certain functions *)
module type ReverseDifferentiation = sig
  include Op
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

(** The main module functor that, given a Value module, produces a Differentiation module with the associated types. *)
module type AD = functor (V : Tensor) -> sig
  include Differentiation with type t = V.t
  include ReverseDifferentiation with type t = V.t
end



