(* Some common errors when operating with n-dimensional tensors *)
module type Errors = sig
  exception DimensionMismatch of string
  exception InvalidArgument of string
  exception DivisionByZero
end
