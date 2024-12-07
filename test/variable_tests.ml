open Core
open OUnit2

module Test = struct
  open Variable


  (* let zero = V.create 0.0
     let one = V.create 1.0 *)
  let a = make 4.0
  let b = make 3.0
  let f1 x y = (x + y) * x

  let test_gradient _ =
    (assert_equal 0.0
    @@
    let g = gradients a in
    let r1 = find g a in
    r1);
    assert_equal [ 11.0; 4.0 ]
    @@
    let f = f1 a b in
    let g = gradients f in
    let d1 = find g a and d2 = find g b in
    [ d1; d2 ]

  let series = "Given tests" >::: [ "gradient" >:: test_gradient ]
end

let series = "Variable tests" >::: [ Test.series ]
let () = run_test_tt_main series
