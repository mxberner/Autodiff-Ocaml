open Core
open Tensor
open Variable

(* Generate synthetic dataset *)
let generate_data (num_samples : int) : v * v * v * v =
  let x1 = random [| num_samples; 2 |] in
  let x2 = random [| num_samples; 2 |] in

  (* Simple XOR-like classification *)
  let y =
    Tensor.map2
      (fun a b ->
        if Float.compare a 0.5 <> Float.compare b 0.5 then 1.0 else 0.0)
      x1.data x2.data
  in
  let y_tensor = make y in

  (x1, x2, y_tensor, x2)

(* Neural network model *)
let create_model () =
  let weights = random [| 2; 2 |] in
  let bias = create 0.0 in
  (weights, bias)

(* Training function *)
let train ?(learning_rate : float = 0.1) ?(epochs : int = 100) (x1 : v) (x2 : v)
    (y_true : v) : unit =
  let weights, bias = create_model () in

  for epoch = 1 to epochs do
    (* Forward pass *)
    let input = matmul x1 weights + x2 in
    let logits = input + bias in
    let y_pred = sigmoid logits in

    (* Compute loss *)
    let loss = binary_cross_entropy y_true y_pred in

    (* Compute gradients *)
    let grad_tbl = gradients loss in

    (* Manual gradient descent *)
    let weights_grad = find grad_tbl weights in
    let bias_grad = find grad_tbl bias in

    (* Update parameters *)
    let _updated_weights =
      make
        (Tensor.sub weights.data
        @@ map (fun g -> learning_rate *. g) weights_grad)
    in

    let _updated_bias =
      make (Tensor.sub bias.data @@ map (fun g -> learning_rate *. g) bias_grad)
    in

    (* Print progress *)
    if Int.(epoch % 10 = 0) then (
      Printf.printf "Epoch %d, Loss: " epoch;
      print loss)
  done

(* Main function *)
let () =
  let num_samples = 1000 in
  let x1, x2, y_true, _ = generate_data num_samples in

  Printf.printf "Starting Neural Network Training\n";
  train x1 x2 y_true;
  Printf.printf "Training Complete\n"
