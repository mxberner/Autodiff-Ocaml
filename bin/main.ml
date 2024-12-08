open Variable
open Core
open Vis

(* Define arithmetic functions for Variable *)
let add_v = add (* Addition for type v *)
let sub_v = sub (* Subtraction for type v *)
let mul_v = mul (* Multiplication for type v *)

(* Adjust f_custom to ensure compatibility with Variable operations *)
let f_custom (a : v) (b : v) : v =
  mul_v
    (mul_v (sub_v (mul_v a b) a) (add_v (add_v b a) (add_v a b)))
    (sub_v a b)

(* Adjust f_float for numerical estimation *)
let f_float (a : float) (b : float) : float =
  ((a *. b) -. a) *. (b +. a +. a +. b) *. (a -. b)

(* Define variables *)
let a_f = 230.3
let b_f = 33.2
let a = make (Scalar a_f)
let b = make (Scalar b_f)
let y = f_custom a b

(* Function to create a computational graph for visualization *)
let build_computation_graph () =
  let nodes = [
    { id = 1; label = "a"; op = None; inputs = [] }; (* Input a *)
    { id = 2; label = "b"; op = None; inputs = [] }; (* Input b *)
    { id = 3; label = "*"; op = Some "*"; inputs = [1; 2] }; (* a * b *)
    { id = 4; label = "-"; op = Some "-"; inputs = [3; 1] }; (* (a * b) - a *)
    { id = 5; label = "+"; op = Some "+"; inputs = [2; 1] }; (* b + a *)
    { id = 6; label = "+"; op = Some "+"; inputs = [5; 1] }; (* (b + a) + a *)
    { id = 7; label = "+"; op = Some "+"; inputs = [6; 2] }; (* (b + a + a) + b *)
    { id = 8; label = "*"; op = Some "*"; inputs = [4; 7] }; (* ((a * b) - a) * (b + a + a + b) *)
    { id = 9; label = "-"; op = Some "-"; inputs = [1; 2] }; (* a - b *)
    { id = 10; label = "*"; op = Some "*"; inputs = [8; 9] }; (* Final: (((a * b) - a) * (b + a + a + b)) * (a - b) *)
    { id = 11; label = "y"; op = None; inputs = [10] }; (* Output y *)
  ] in
  visualize_computation_graph nodes

(* Autodiff gradient results *)
let auto_diff_result () =
  print_endline "Autodiff Gradient Result";
  (* Compute gradients *)
  let g_tbl = gradients y in

  (* Get the gradient of d with respect to a *)
  let grad_a = find g_tbl a and grad_b = find g_tbl b in
  match (grad_a, grad_b) with
  | Scalar dfda, Scalar dfdb ->
      (* Print the partial derivative *)
      Printf.printf "The partial derivative of y with respect to a = %f\n" dfda;
      (* Print the partial derivative *)
      Printf.printf "The partial derivative of y with respect to b = %f\n" dfdb
  | _ -> failwith "Error"

(* Numerical estimation of gradients *)
let numerical_estimate_result () =
  print_endline "Numerical Estimation (check the result of auto grad)";
  let delta = 0.000001 in
  let grad_a = (f_float (a_f +. delta) b_f -. f_float a_f b_f) /. delta in
  (* Print the partial derivative *)
  Printf.printf "The partial derivative of y with respect to a = %f\n" grad_a;
  let grad_b = (f_float a_f (b_f +. delta) -. f_float a_f b_f) /. delta in
  Printf.printf "The partial derivative of y with respect to b = %f\n" grad_b;
  print_endline ""

(* Main execution *)
let () =
  (* Generate and visualize the computational graph *)
  build_computation_graph ();

  (* Compute and display results *)
  match y.value with
  | Scalar v ->
      Printf.printf "y.value = %f\n" v;
      auto_diff_result ();
      numerical_estimate_result ()
  | _ -> failwith "Error"
