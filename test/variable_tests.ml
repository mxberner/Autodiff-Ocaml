open Core
open OUnit2
open Variable

let a = make @@ Scalar 4.0
let b = make @@ Scalar 3.0
let f1 x y = (x + y) * x
let f2 x = sin x
let f3 x = x * sin x

module Test = struct
  open Tensor

  let test_gradient _ =
    assert_equal (Scalar 0.0) @@ find (gradients a) a;
    (assert_equal [ Scalar 11.0; Scalar 4.0 ]
    @@
    let f = f1 a b in
    let g = gradients f in
    let dfda = find g a and dfdb = find g b in
    [ dfda; dfdb ]);
    (assert_equal (Scalar 0.540302305868139765)
    @@
    let f = f2 one in
    let dfdx = find (gradients f) one in
    dfdx);
    assert_equal (Scalar (-3.37137697876237574))
    @@
    let x = make (Scalar 4.0) in
    let f = f3 x in
    let dfdx = find (gradients f) x in
    dfdx

  let series = "Given tests" >::: [ "gradient" >:: test_gradient ]
end

let series = "Variable tests" >::: [ Test.series ]
let () = run_test_tt_main series
