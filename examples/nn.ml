open Core
open Variable

(* Assume that 'v' type and all the operations from your code snippet are in scope *)
(* We have: type v = { id : int; mutable data : t; local_gradients : (v * (t -> t)) list } *)
(* and functions like make, add, mul, matmul, sigmoid, binary_cross_entropy, etc. *)

(* Simple neural network parameters *)
(* Let's say we have an input of shape (N, D_in) and a single hidden layer of H,
   and an output dimension of 1 for binary classification *)

let n = 4 (* number of examples *)
let d_in = 3 (* input features *)
let h = 5 (* hidden layer size *)
let d_out = 1 (* output dimension *)

(* Create random inputs and targets *)
let x_data = Tensor.random ~seed:42 [| n; d_in |]
let y_data = Tensor.random ~seed:0 [| n; d_out |]

(* Convert targets to a binary form (0/1) just for demonstration *)
let y_data = Tensor.map (fun e -> if Float.(e > 0.5) then 1.0 else 0.0) y_data

(* Wrap inputs into variables *)
let x = make x_data
let y = make y_data

(* Initialize weights *)
let w = make (Tensor.random ~seed:1 [| d_in; d_out |])

(* Forward pass function *)
let forward x =
  (* Hidden layer: x * w1 , then leaky_relu *)
  let hidden = leaky_relu (matmul x w) in
  sigmoid hidden

(* Training step *)
let () =
  (* Forward pass *)
  let y_pred = forward x in
  (* Compute loss *)
  let loss = binary_cross_entropy y  y_pred in
  Printf.printf "Loss before backprop: ";
  print loss;
  (* Print the loss *)
  Printf.printf "\n";

  (* Compute gradients *)
  let grad_tbl = gradients loss in

  (* Fetch gradients for w for demonstration *)
  let dw = find grad_tbl w in

  Printf.printf "Gradients:\n";
  Printf.printf "dw: ";
  Tensor.print dw;
  Printf.printf "\n";

  (* Optional: simple gradient descent step *)
  (* Let's choose a learning rate *)
  let lr = 0.01 in
  w.data <- Tensor.(w.data - (dw * Tensor.create ~dims:(shape dw) lr));

  (* Forward pass after update *)
  let y_pred_after = forward x in
  let loss_after = binary_cross_entropy y y_pred_after in

  Printf.printf "\nLoss after backprop and parameter update: ";
  print loss_after;
  Printf.printf "\n"
