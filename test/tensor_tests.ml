open Core
open OUnit2

module Test = struct
  module T = Tensor
  open Variable

  let get_shape a b : T.s = { rows = a; cols = b }
  let empty = T.Vector (Array.of_list [])
  let zeros_vector = T.Vector (Array.of_list [ zero; zero; zero; zero ])

  let zeros_matrix =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ zero; zero; zero ];
           Array.of_list [ zero; zero; zero ];
           Array.of_list [ zero; zero; zero ];
           Array.of_list [ zero; zero; zero ];
         ])

  let ones_vector = T.Vector (Array.of_list [ one; one; one; one ])

  let ones_matrix =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ one; one; one ];
           Array.of_list [ one; one; one ];
           Array.of_list [ one; one; one ];
           Array.of_list [ one; one; one ];
         ])

  let test_matrix_1 =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ one; make 2.0; make 3.0 ];
           Array.of_list [ make 3.0; make 2.0; one ];
           Array.of_list [ one; make 2.0; make 3.0 ];
         ])

  let test_matrix_2 =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ make 4.0; make 5.0; make 6.0 ];
           Array.of_list [ make 6.0; make 5.0; make 4.0 ];
           Array.of_list [ make 4.0; make 6.0; make 5.0 ];
         ])

  let test_matrix_3 =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ make 28.0; make 33.0; make 29.0 ];
           Array.of_list [ make 28.0; make 31.0; make 31.0 ];
           Array.of_list [ make 28.0; make 33.0; make 29.0 ];
         ])

  let test_matrix_4 =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ one; make 2.0; make 3.0 ];
           Array.of_list [ one; make 2.0; make 3.0 ];
         ])

  let test_matrix_5 =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ make 4.0 ];
           Array.of_list [ make 6.0 ];
           Array.of_list [ make 4.0 ];
         ])

  let test_matrix_6 =
    T.Matrix
      (Array.of_list
         [ Array.of_list [ make 28.0 ]; Array.of_list [ make 28.0 ] ])

  let test_1_shape _ =
    assert_equal (get_shape 0 0) @@ T.shape empty;
    assert_equal (get_shape 4 0) @@ T.shape zeros_vector;
    assert_equal (get_shape 4 3) @@ T.shape zeros_matrix

  let test_2_zeros _ =
    assert_equal empty @@ T.zeros [];
    assert_equal zeros_vector @@ T.zeros [ 4 ];
    assert_equal zeros_matrix @@ T.zeros [ 4; 3 ]

  let test_3_ones _ =
    assert_equal empty @@ T.ones [];
    assert_equal ones_vector @@ T.ones [ 4 ];
    assert_equal ones_matrix @@ T.ones [ 4; 3 ]

  let test_4_random _ =
    assert_equal (get_shape 0 0) @@ T.shape @@ T.random [];
    assert_equal (get_shape 2 0) @@ T.shape @@ T.random [ 2 ];
    assert_equal (get_shape 4 4) @@ T.shape @@ T.random [ 4; 4 ]

  let test_5_map _ =
    let f x = x + one in
    assert_equal empty @@ T.map f empty;
    assert_equal ones_vector @@ T.map f zeros_vector;
    assert_equal ones_matrix @@ T.map f zeros_matrix

  let test_6_map2 _ =
    let f x y = x - y in
    assert_equal empty @@ T.map2 f empty empty;
    assert_equal zeros_vector @@ T.map2 f ones_vector ones_vector;
    assert_equal zeros_matrix @@ T.map2 f ones_matrix ones_matrix

  let test_7_dot _ =
    let product a b =
      let res = T.dot a b in
      match res with Scalar s -> s.value | _ -> failwith "error"
    in
    assert_equal 0.0 @@ product zeros_vector zeros_vector;
    assert_equal 0.0 @@ product ones_vector zeros_vector;
    assert_equal 4.0 @@ product ones_vector ones_vector

  let test_8_matmul _ =
    assert_equal test_matrix_3 @@ T.matmul test_matrix_1 test_matrix_2;
    assert_equal test_matrix_6 @@ T.matmul test_matrix_4 test_matrix_5

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
