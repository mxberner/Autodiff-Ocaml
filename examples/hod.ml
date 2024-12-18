(* Higher order derivative example*)
open Core
open Variable

let x = make @@ Tensor.create 4.0
let f x = x * x * x
let y = f x

let () =
  (* Compute gradients *)
  Printf.printf "f (x = 4.0) = x ** 3 = %f\n" @@ Tensor.get y.data [||];
  let f' = gradients y in
  (* Get the derivative of f with respect to x *)
  let f'_value = find f' x in
  Printf.printf "f'(x = 4.0) = 3 * x ** 2 = %f\n" @@ Tensor.get f'_value [||];
  let f'' = gradients y in
  (* Get the 2nd derivative of f with respect to x *)
  let f''_value = find f'' x in
  Printf.printf "f''(x = 4.0) = 6 * x = %f\n" @@ Tensor.get f''_value [||];
  print_table f''
