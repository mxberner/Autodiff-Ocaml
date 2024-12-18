open Core
module V = Variable

let input_size = 1
let output_size = 1
(* let learning_rate = 0.001 *)

let () =
  let x = V.random ~seed:0 [| 1; input_size |] in
  let y = V.random ~seed:1 [| 1; output_size |] in
  let w = ref (V.random ~seed:2 [| input_size; output_size |]) in
  Printf.printf "x: %f\n" @@ V.get x [| 0; 0 |];
  Printf.printf "y: %f\n" @@ V.get y [| 0; 0|];
  Printf.printf "w: %f\n" @@ V.get !w [| 0; 0 |];
  
