open Core
open OUnit2
open Tensor

module Test = struct
  module V = Variable

  let zero = Scalar (V.zero ())
  let one = Scalar (V.one ())
  let get_shape a b : dimension = { rows = a; cols = b }
  let zeros_vector = Vector (Array.init 4 ~f:(fun _ -> V.zero ()))

  let zeros_matrix =
    Matrix
      (Array.of_list
         [
           Array.of_list [ V.zero (); V.zero (); V.zero () ];
           Array.of_list [ V.zero (); V.zero (); V.zero () ];
           Array.of_list [ V.zero (); V.zero (); V.zero () ];
           Array.of_list [ V.zero (); V.zero (); V.zero () ];
         ])

  let ones_vector = Vector (Array.init 4 ~f:(fun _ -> V.one ()))

  let ones_matrix =
    Matrix
      (Array.of_list
         [
           Array.of_list [ V.one (); V.one (); V.one () ];
           Array.of_list [ V.one (); V.one (); V.one () ];
           Array.of_list [ V.one (); V.one (); V.one () ];
           Array.of_list [ V.one (); V.one (); V.one () ];
         ])

  let test_matrix_1 =
    Matrix
      (Array.of_list
         [
           Array.of_list [ V.one (); V.make 2.0; V.make 3.0 ];
           Array.of_list [ V.make 3.0; V.make 2.0; V.one () ];
           Array.of_list [ V.one (); V.make 2.0; V.make 3.0 ];
         ])

  let test_matrix_2 =
    Matrix
      (Array.of_list
         [
           Array.of_list [ V.make 4.0; V.make 5.0; V.make 6.0 ];
           Array.of_list [ V.make 6.0; V.make 5.0; V.make 4.0 ];
           Array.of_list [ V.make 4.0; V.make 6.0; V.make 5.0 ];
         ])

  let test_matrix_3 =
    Matrix
      (Array.of_list
         [
           Array.of_list [ V.make 28.0; V.make 33.0; V.make 29.0 ];
           Array.of_list [ V.make 28.0; V.make 31.0; V.make 31.0 ];
           Array.of_list [ V.make 28.0; V.make 33.0; V.make 29.0 ];
         ])

  let test_matrix_4 =
    Matrix
      (Array.of_list
         [
           Array.of_list [ V.one (); V.make 2.0; V.make 3.0 ];
           Array.of_list [ V.one (); V.make 2.0; V.make 3.0 ];
         ])

  let test_matrix_5 =
    Matrix
      (Array.of_list
         [
           Array.of_list [ V.make 4.0 ];
           Array.of_list [ V.make 6.0 ];
           Array.of_list [ V.make 4.0 ];
         ])

  let test_matrix_6 =
    Matrix
      (Array.of_list
         [ Array.of_list [ V.make 28.0 ]; Array.of_list [ V.make 28.0 ] ])

  let test_1_shape _ =
    assert_equal (get_shape 0 0) @@ shape @@ Scalar (V.zero ());
    assert_equal (get_shape 4 1) @@ shape zeros_vector;
    assert_equal (get_shape 4 3) @@ shape zeros_matrix

  let test_2_zeros _ =
    assert_bool "t21" @@ equal zero @@ zeros [];
    assert_bool "t22" @@ equal zeros_vector @@ zeros [ 4 ];
    assert_bool "t23" @@ equal zeros_matrix @@ zeros [ 4; 3 ]

  let test_3_ones _ =
    assert_bool "t31" @@ equal one @@ ones [];
    assert_bool "t32" @@ equal ones_vector @@ ones [ 4 ];
    assert_bool "t33" @@ equal ones_matrix @@ ones [ 4; 3 ]

  let test_4_random _ =
    assert_equal (get_shape 0 0) @@ shape @@ random [];
    assert_equal (get_shape 2 1) @@ shape @@ random [ 2 ];
    assert_equal (get_shape 4 4) @@ shape @@ random [ 4; 4 ]

  let test_5_map _ =
    let f x = V.add x (V.one ()) in
    assert_bool "" @@ equal one (map f zero);
    assert_bool "" @@ equal ones_vector (map f zeros_vector)

  let test_6_map2 _ =
    let f x y = V.sub x y in
    assert_bool "" @@ equal zero @@ map2 f zero zero;
    assert_bool "" @@ equal zeros_vector @@ map2 f ones_vector ones_vector;
    assert_bool "" @@ equal zeros_matrix @@ map2 f ones_matrix ones_matrix

  let test_7_dot _ =
    let product a b =
      let res = dot a b in
      match res with Scalar s -> s.value | _ -> failwith "error"
    in
    assert_equal 0.0 @@ product zeros_vector zeros_vector;
    assert_equal 0.0 @@ product ones_vector zeros_vector;
    assert_equal 4.0 @@ product ones_vector ones_vector

  let test_8_matmul _ =
    assert_bool "" @@ equal test_matrix_3 @@ matmul test_matrix_1 test_matrix_2;
    assert_bool "" @@ equal test_matrix_6 @@ matmul test_matrix_4 test_matrix_5

  let series =
    "Given tests"
    >::: [
           "1 - shape" >:: test_1_shape;
           "2 - zeros" >:: test_2_zeros;
           "3 - ones" >:: test_3_ones;
           "4 - random" >:: test_4_random;
           "5 - map" >:: test_5_map;
           "6 - map2" >:: test_6_map2;
           "7 - dot" >:: test_7_dot;
           "8 - matmul" >:: test_8_matmul;
         ]
end

let series = "Tensor tests" >::: [ Test.series ]
let () = run_test_tt_main series
