open Core
open Variable

let f1_custom a b = (a * (a / b)) + (a + b)
let a = make @@ Tensor.create 230.3
let b = make @@ Tensor.create 33.2 

let auto_diff_result f =
  print_endline "Autodiff Gradient Result";
  (* Compute gradients *)
  let g_tbl = gradients f in

  (* Get the gradient of d with respect to a *)
  let grad_a = find g_tbl a and grad_b = find g_tbl b in
  match (grad_a, grad_b) with
  | dfda, dfdb ->
      (* Print the partial derivative *)
      Printf.printf "The partial derivative of y with respect to a = %f\n"
      @@ Tensor.get dfda [||];
      (* Print the partial derivative *)
      Printf.printf "The partial derivative of y with respect to b = %f\n"
      @@ Tensor.get dfdb [||]; 
      print_table g_tbl

      
let () =
  print_endline
    "****************************************************************************************";
  Printf.printf "Add a bit of explanation here.";
  auto_diff_result (f1_custom a b); 
  print_endline
    "****************************************************************************************"