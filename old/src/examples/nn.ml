open Core
open Tensor
module V = Variable

let seed = 0
let input_size = 5
let output_size = 100
let learning_rate = 0.001

let () =
  let x = random ~seed [ 1; input_size ] in
  let y = random ~seed [ 1; output_size ] in
  let w = ref (random ~seed [ input_size; output_size ]) in
  for i = 1 to 100 do
    let y_pred = matmul x w.contents in
    let loss = sum (pow (y - y_pred) (Scalar (V.make 2.0))) in
    if Int.(i % 10 = 0) then ( 
      Printf.printf "[%d/100] Loss: %f\n" i loss.value;
      print_endline ""
    );

    let grad = V.gradients loss in
    w :=
      map
        (fun w_ ->
          let g = V.find grad w_ in
          V.sub w_ (V.make (g.value *. learning_rate)))
        w.contents
  done
