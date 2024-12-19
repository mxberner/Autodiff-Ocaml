open Tensor

[@@@ocaml.warning "-27"]

type f = t -> t
type optimizer = { init : t; step : t -> t; stop_condition : t -> bool }

let gradient_descent f opt = failwith "unimplemented"
