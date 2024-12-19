(*
   tensor.mli
   This module defines operations on tensors, which can be scalars, vectors, or matrices. *)
open Variable

type t = Scalar of v | Vector of v array | Matrix of v array array
(* Define tensor type, which can be a Scalar, Vector, or Matrix *)

exception DimensionMismatch of string

type dims = { rows : int; cols : int }
(* Define type for representing shape of tensors (rows and columns) *)

(** [t] is a type representing a tensor, which can either be a Scalar (a single number),
    a Vector (a 1D array of numbers), or a Matrix (a 2D array of numbers).
    [s] represents the shape of a tensor, number of rows and columns. *)

val shape : t -> dims
(** [shape tensor] returns the dimensions of the tensor, 
    record with [rows] and [cols] fields. *)

val zeros : int list -> t
(** [zeros dims] creates a tensor (scalar, vector, or matrix) with the specified 
    dimensions, filled with zeros. 

    {examples}
    - [zeros [3]] creates a vector of length 3 filled with zeros, 
    - [zeros [2; 3]] creates a 2x3 matrix filled with zeros. *)

val ones : int list -> t
(** [ones dims] creates a tensor (scalar, vector, or matrix) with the specified 
    dimensions, filled with ones. 

    {examples}
    - [ones [3]] creates a vector of length 3 filled with ones, 
    - [ones [2; 3]] creates a 2x3 matrix filled with ones. *)

val random : ?seed:int -> int list -> t
(** [random ?seed dims] creates a tensor (scalar, vector, or matrix) with random 
    values, where [dims] specifies the dimensions of the tensor. 
    Optionally, [seed] can be provided to control the randomness. *)

val map : (v -> v) -> t -> t
(** [map f tensor] applies the function [f] element-wise to each element of the 
    tensor [tensor], and returns a new tensor with the results. *)

val map2 : (v -> v -> v) -> t -> t -> t
(** [map2 f tensor1 tensor2] applies the function [f] element-wise to corresponding 
    elements of [tensor1] and [tensor2], and returns a new tensor with the results.*)

val add : t -> t -> t
(** [add tensor1 tensor2] performs element-wise addition of the two tensors.*)

val sub : t -> t -> t
(** [sub tensor1 tensor2] performs element-wise subtraction of the second tensor 
    from the first.*)

val mul : t -> t -> t
(** [mul tensor1 tensor2] performs element-wise multiplication of the two tensors.*)

(* val div : t -> float -> t *)
(** [div tensor scalar] divides each element of the tensor by the scalar value. 
    Raises an exception if attempting to divide by zero. *)

val pow : t -> t -> t
(** [pow tensor exponent] raises each element of the tensor to the power of [exponent]. *)

val log : t -> t
(** [log tensor] applies the natural logarithm element-wise to each element of the tensor. 
    This is only valid for positive values. *)

val exp : t -> t
(** [exp tensor] applies the exponential function (e^x) element-wise to each element of 
    the tensor. *)

val sin : t -> t
(** [sin tensor] applies the sine function element-wise to each element of the tensor. *)

val cos : t -> t
(** [cos tensor] applies the cosine function element-wise to each element of the tensor. *)

val tan : t -> t
(** [tan tensor] applies the tangent function element-wise to each element of the tensor. *)

val equal : t -> t -> bool
(** [equal tensor1 tensor2] checks whether the two tensors are element-wise equal. 
    Returns true if they are equal, false otherwise. *)

val dot : t -> t -> t
(** [dot tensor1 tensor2] computes the dot product of two vectors. *)

val matmul : t -> t -> t
(** [matmul tensor1 tensor2] performs matrix multiplication on two matrices. 
    The number of columns in the first matrix must match the number of rows in the 
    second matrix. *)

val reshape : t -> int list -> t
(** [reshape tensor dims] reshapes the tensor to the specified dimensions. 
    UNSUPPORTED *)

val transpose : t -> t
(** [transpose tensor] transposes the tensor. This operation is only applicable for matrices, 
    swapping rows and columns. *)

val neg : t -> t
(** [neg tensor] negates each element of the tensor, i.e., multiplies each element by -1. *)

val flatten : t -> t
(** [flatten tensor] flattens the tensor into a one-dimensional vector, regardless of its 
    original shape. *)

val sum : t -> v
(** [sum tensor] computes the sum of all elements in the tensor, returning a scalar value. *)

val print : t -> unit

(* Operator overloading *)
val ( + ) : t -> t -> t
(** [tensor1 + tensor2] performs element-wise addition of the two tensors. *)

val ( - ) : t -> t -> t
(** [tensor1 - tensor2] performs element-wise subtraction of the two tensors. *)

val ( * ) : t -> t -> t
(** [tensor1 * tensor2] performs element-wise multiplication of the two tensors. *)

(* val ( / ) : t -> float -> t *)
(** [tensor / scalar] divides each element of the tensor by the scalar. *)

val ( = ) : t -> t -> bool
(** [tensor1 = tensor2] checks whether the two tensors are element-wise equal. *)
