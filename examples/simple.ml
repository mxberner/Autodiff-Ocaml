open Variable

let f_custom a b = ((a * b) - a) * (b + a + a + b) * (a - b)
let f_float a b = ((a *. b) -. a) *. (b +. a +. a +. b) *. (a -. b)
let a_f = 230.3
let a = make 230.3
let b_f = 33.2
let b = make 33.2
let y = f_custom a b

let auto_diff_result () =
  print_endline "Autodiff Gradient Result";
  (* Compute gradients *)
  let g_tbl = gradients y in

  (* Get the gradient of d with respect to a *)
  let grad_a = find g_tbl a and grad_b = find g_tbl b in
  match (grad_a, grad_b) with
  | dfda, dfdb ->
      (* Print the partial derivative *)
      Printf.printf "The partial derivative of y with respect to a = %f\n" dfda;
      (* Print the partial derivative *)
      Printf.printf "The partial derivative of y with respect to b = %f\n" dfdb

let numerical_estimate_result () =
  print_endline "Numerical Estimation (check the result of auto grad)";
  let delta = 0.000001 in
  let grad_a = (f_float (a_f +. delta) b_f -. f_float a_f b_f) /. delta in
  (* Print the partial derivative *)
  Printf.printf "The partial derivative of y with respect to a = %f\n" grad_a;
  let grad_b = (f_float a_f (b_f +. delta) -. f_float a_f b_f) /. delta in
  Printf.printf "The partial derivative of y with respect to b = %f\n" grad_b;
  print_endline ""

let () =
  match y.value with
  | v ->
      Printf.printf "y.value = %f\n" v;
      auto_diff_result ();
      numerical_estimate_result ()
