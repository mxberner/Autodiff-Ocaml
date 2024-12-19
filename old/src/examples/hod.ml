(* Higher order derivative example*)
open Core
open Variable

let x = make 4.0
let f x = x * x * x
let y = f x

let () =
  (* Compute gradients *)
  Printf.printf "f (x = 4.0) = x ** 3 = %f\n" y.value;
  let f' = gradients y in
  (* Get the derivative of f with respect to x *)
  let f'_value = find f' x in
  Printf.printf "f'(x = 4.0) = 3 * x ** 2 = %f\n" f'_value.value;
  let f'' = gradients f'_value in
  (* Get the 2nd derivative of f with respect to x *)
  let f''_value = find f'' x in
  Printf.printf "f''(x = 4.0) = 6 * x = %f\n" f''_value.value
