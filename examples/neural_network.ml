open Core
open Tensor
module V = Variable

let seed = 0
let input_size = 2
let output_size = 2
let learning_rate = 0.001

let () =
  let x = random ~seed [ 1; input_size ] in
  let y = random ~seed [ 1; output_size ] in
  let w = ref (random ~seed [ input_size; output_size ]) in
  for _ = 1 to 10 do
    let y_pred = matmul x w.contents in
    let loss = sum (pow (y - y_pred) 2.0) in
    Printf.printf "Loss: %f \n" loss.value;
    let grad = V.gradients loss in
    w :=
      map
        (fun w_ ->
          let g = V.find grad w_ in
          V.sub w_ (V.make (g *. learning_rate)))
        w.contents
  done
