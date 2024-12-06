(* errors.ml *)

module Errors = struct
  exception DimensionMismatch of string
  exception InvalidArgument of string
  exception DivisionByZero
end