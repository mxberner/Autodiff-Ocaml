(* open Variable

let f a b = ((a / b) - a) * ((b / a) + a + b) * (a - b)
let a = make 230.3
let b = make 33.2
let y = f a b

let auto_diff_result () =
  print_endline "Autodiff Gradient Result";
  (* Compute gradients *)
  let g_tbl = gradients y in

  (* Get the gradient of d with respect to a *)
  let grad_a = find g_tbl a in
  match grad_a with | Scalar dfda ->
  (* Print the partial derivative *)
  Printf.printf "The partial derivative of y with respect to a = %f\n" dfda;
  let grad_b = find g_tbl b in
  Printf.printf "The partial derivative of y with respect to b = %f\n" grad_b;
  print_endline ""

let numerical_estimate_result () =
  print_endline "Numerical Estimation (check the result of auto grad)";
  let delta = make 0.000001 in
  let grad_a = (f (a + delta) b - f a b) / delta in
  (* Print the partial derivative *)
  Printf.printf "The partial derivative of y with respect to a = %f\n"
    grad_a.value;
  let grad_b = (f a (b + delta) - f a b) / delta in
  Printf.printf "The partial derivative of y with respect to b = %f\n"
    grad_b.value;
  print_endline ""

let () =
  Printf.printf "y.value = %f\n" y.value;
  auto_diff_result ();
  numerical_estimate_result () *)
