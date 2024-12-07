open Variable

(* The main data type we'll run operations on *)
type t = Scalar of v | Vector of v array | Matrix of v array array
type s = { rows : int; cols : int }

val shape : t -> s
(** The type representing numerical values, which could be scalars, vectors, or matrices. *)

(** Returns the dimensions of the value {rows; cols}. *)

val zeros : int list -> t
(** Creates a n x n tensor value filled with zeros, given the specified dimensions. *)

val ones : int list -> t
(** Creates a n x n tensor value filled with ones, given the specified dimensions. *)

val random : ?seed:int -> int list -> t
(** Generates a value with random entries, given the specified dimensions and an optional seed. *)

val map : (v -> v) -> t -> t

(* map f a applies function f to all the elements of a, and builds an array with the results returned by f: [| f a.(0); f a.(1); ...; f a.(length a - 1) |]. *)
val map2 : (v -> v -> v) -> t -> t -> t
(* map2 f a b applies function f to all the elements of a and b, and builds an array with the results returned by f: [| f a.(0) b.(0); ...; f a.(length a - 1) b.(length b - 1)|]. *)

val add : t -> t -> t
(** Element-wise addition of two values. Raises DimensionMismatch if shapes are incompatible. *)

val sub : t -> t -> t
(** Element-wise subtraction of two values. Raises DimensionMismatch if shapes are incompatible. *)

val mul : t -> t -> t
(** Element-wise multiplication of two values. Raises DimensionMismatch if shapes are incompatible. *)

val div : t -> float -> t
(** Element-wise division of two values. Raises DivisionByZero*)

val less : t -> t -> t
(** Element-wise less than*)

val equal : t -> t -> t

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

val neg : t -> t
(** Negates each element of the value. *)

val flatten : t -> t
(** Flattens the value into a one-dimensional array. *)

val sum : t -> float
(** Sums all elements of the value, returning a scalar value. *)

(* Operator overloading for custom operations on matrices *)
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> float -> t
