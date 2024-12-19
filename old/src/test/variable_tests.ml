open Core
open OUnit2
open Variable

let x = make @@ 4.0
let y = make @@ 3.0

module Test = struct
  let test_add _ =
    let f x y = x + y in
    let g = gradients (f x y) in
    let res = find g x and dfdy = find g y in
    assert_equal 1.0 res.value;
    assert_equal 1.0 dfdy.value;
    let f x y = x + x + y + y + y in
    let g = gradients (f x y) in
    let res = find g x and dfdy = find g y in
    assert_equal 2.0 res.value;
    assert_equal 3.0 dfdy.value

  let test_mul _ =
    let f x y = x * y in
    let g = gradients (f x y) in
    let res = find g x and dfdy = find g y in
    assert_equal 3.0 res.value;
    assert_equal 4.0 dfdy.value;
    let f x y = x * x * y * y * y in
    let g = gradients (f x y) in
    let res = find g x and dfdy = find g y in
    assert_equal 216.0 res.value;
    assert_equal 432.0 dfdy.value

  let test_div _ =
    let g = gradients (x / y) in
    let res = find g x and dfdy = find g y in
    assert_equal (1.0 /. 3.0) res.value;
    assert_equal (-4.0 /. 9.0) dfdy.value;
    let g = gradients (x / x / y) in
    let res = find g x and dfdy = find g y in
    assert_equal 0.0 res.value;
    assert_equal (-1.0 /. 9.0) dfdy.value

  let test_neg _ =
    let f' = gradients (neg x) in
    let res = find f' x in
    assert_equal (-1.0) res.value;
    (* Double derivative *)
    let f'' = gradients (find f' x) in
    let res = find f'' x in
    assert_equal 0.0 res.value

  let test_inv _ =
    let f' = gradients (inv x) in
    let res = find f' x in
    assert_equal (-0.0625) res.value;
    (* Double derivative *)
    let f'' = gradients (find f' x) in
    let res = find f'' x in
    assert_equal 0.03125 res.value

  let test_pow _ =
    let f' = gradients (pow x y) in
    let res = find f' x in
    assert_equal 48.0 res.value;
    (* Double derivative *)
    let f'' = gradients (find f' x) in
    let res = find f'' x in
    assert_equal 24.0 res.value

  let test_sin _ =
    let f' = gradients (sin x) in
    let res = find f' x in
    assert_equal (-0.65364362086361194) res.value

  let test_cos _ =
    let f' = gradients (cos x) in
    let res = find f' x in
    assert_equal 0.756802495307928202 res.value

  let test_tan _ =
    let f' = gradients (tan x) in
    let res = find f' x in
    assert_equal 2.34055012186162 res.value 

  let test_log _ =
    let f' = gradients (log x) in
    let res = find f' x in
    assert_equal 0.25 res.value 

  let test_exp _ =
    let f' = gradients (exp x) in
    let res = find f' x in
    assert_equal 54.598150033144236 res.value 

  let test_print _ =
    let res = print x in
    assert_equal (Printf.printf "4.0") res 

  let series =
    "Given tests"
    >::: [
           "1 - add" >:: test_add;
           "2 - mul" >:: test_mul;
           "3 - div" >:: test_div;
           "4 - neg" >:: test_neg;
           "5 - inv" >:: test_inv;
           "6 - pow" >:: test_pow;
           "7 - sin" >:: test_sin;
           "8 - cos" >:: test_cos;
           "9 - tan" >:: test_tan; 
           "10 - log" >:: test_log;
           "11 - exp" >:: test_exp;
           "12 - print" >:: test_print;
         ]
end

let series = "Variable tests" >::: [ Test.series ]
let () = run_test_tt_main series
