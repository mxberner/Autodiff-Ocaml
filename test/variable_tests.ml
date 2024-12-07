open Core
open OUnit2

module Test = struct
  open Variable

  (* let zero = V.create 0.0
     let one = V.create 1.0 *)
  let a = create 4.0
  let b = create 3.0
  let f1 x y = (x + y) * x

  let test_gradient _ =
    assert_equal 28.0
    @@
    let f = f1 a b in
    let g = gradients f in
    let res = find g a in
    res

  let series = "Given tests" >::: [ "gradient" >:: test_gradient ]
end

let series = "Variable tests" >::: [ Test.series ]
let () = run_test_tt_main series
