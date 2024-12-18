open Core
open OUnit2
open Variable

let x = make @@ Tensor.create 4.0
let y = make @@ Tensor.create 3.0
let tolerance = 0.00001

module Test = struct
  module T = Tensor

  let test_add _ =
    let f x y = x + y in
    let g = gradients (f x y) in
    let dfdx = find g x and dfdy = find g y in
    assert_equal ~printer:string_of_float 1.0 @@ T.get dfdx.data [||];
    assert_equal ~printer:string_of_float 1.0 @@ T.get dfdy.data [||];
    let f x y = x + x + y + y + y in
    let g = gradients (f x y) in
    let dfdx = find g x and dfdy = find g y in
    assert_equal ~printer:string_of_float 2.0 @@ T.get dfdx.data [||];
    assert_equal ~printer:string_of_float 3.0 @@ T.get dfdy.data [||]

  let test_mul _ =
    let f x y = x * y in
    let g = gradients (f x y) in
    let dfdx = find g x and dfdy = find g y in
    assert_equal ~printer:string_of_float 3.0 @@ T.get dfdx.data [||];
    assert_equal ~printer:string_of_float 4.0 @@ T.get dfdy.data [||];
    let f x y = x * x * y * y * y in
    let g = gradients (f x y) in
    let dfdx = find g x and dfdy = find g y in
    assert_equal ~printer:string_of_float 216.0 @@ T.get dfdx.data [||];
    assert_equal ~printer:string_of_float 432.0 @@ T.get dfdy.data [||]

  let test_div _ =
    let g = gradients (x / y) in
    let dfdx = find g x and dfdy = find g y in
    assert_bool "dfdx"
      (Float.( < ) (Float.abs ((1.0 /. 3.0) -. T.get dfdx.data [||])) tolerance);
    assert_bool "dfdy"
      (Float.( < )
         (Float.abs ((-4.0 /. 9.0) -. T.get dfdy.data [||]))
         tolerance);

    let g = gradients (x / x / y) in
    let dfdx = find g x and dfdy = find g y in
    assert_equal ~printer:string_of_float 0.0 (T.get dfdx.data [||]);
    assert_bool "dfdy"
      (Float.( < )
         (Float.abs ((-1.0 /. 9.0) -. T.get dfdy.data [||]))
         tolerance)

  let test_neg _ =
    let f' = gradients (neg x) in
    let res = find f' x in
    assert_equal ~printer:string_of_float (-1.0) @@ T.get res.data [||];
    (* Double derivative *)
    let f'' = gradients (find f' x) in
    let res = find f'' x in
    assert_equal ~printer:string_of_float 0.0 @@ T.get res.data [||]

  let test_inv _ =
    let f' = gradients (inv x) in
    let res = find f' x in
    assert_equal ~printer:string_of_float (-0.0625) @@ T.get res.data [||];
    (* Double derivative *)
    let f'' = gradients (find f' x) in
    let res = find f'' x in
    assert_equal ~printer:string_of_float 0.03125 @@ T.get res.data [||]


  let test_sin _ =
    let f' = gradients (sin x) in
    let dfdx = find f' x in
    assert_bool "dfdx"
      (Float.( < )
         (Float.abs (-0.653643620864 -. T.get dfdx.data [||]))
         tolerance)

  let test_cos _ =
    let f' = gradients (cos x) in
    let dfdx = find f' x in
    assert_bool "dfdx"
      (Float.( < )
         (Float.abs (0.756802495308 -. T.get dfdx.data [||]))
         tolerance)

  let test_tan _ =
    let f' = gradients (tan x) in
    let dfdx = find f' x in
    assert_bool "dfdx"
      (Float.( < )
         (Float.abs (2.34055012186162 -. T.get dfdx.data [||]))
         tolerance)

  let test_log _ =
    let f' = gradients (log x) in
    let dfdx = find f' x in
    assert_equal 0.25 @@ T.get dfdx.data [||]

  let test_exp _ =
    let f' = gradients (exp x) in
    let dfdx = find f' x in
    assert_bool "dfdx"
      (Float.( < )
         (Float.abs (54.598150033144236 -. T.get dfdx.data [||]))
         tolerance)

  let series =
    "Given tests"
    >::: [
           "1 - add" >:: test_add;
           "2 - mul" >:: test_mul;
           "3 - div" >:: test_div;
           "4 - neg" >:: test_neg;
           "5 - inv" >:: test_inv;
           "6 - sin" >:: test_sin;
           "7 - cos" >:: test_cos;
           "8 - tan" >:: test_tan;
           "9 - log" >:: test_log;
           "10 - exp" >:: test_exp;
         ]
end

let series = "Variable tests" >::: [ Test.series ]
let () = run_test_tt_main series
