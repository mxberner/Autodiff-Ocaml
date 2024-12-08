open Core
open OUnit2

module Test = struct
  module T = Tensor

  let get_shape a b : T.s = { rows = a; cols = b }
  let empty = T.Vector (Array.of_list [])
  let zeros_vector = T.Vector (Array.of_list [ 0.0; 0.0; 0.0; 0.0 ])

  let zeros_matrix =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ 0.0; 0.0; 0.0 ];
           Array.of_list [ 0.0; 0.0; 0.0 ];
           Array.of_list [ 0.0; 0.0; 0.0 ];
           Array.of_list [ 0.0; 0.0; 0.0 ];
         ])

  let ones_vector = T.Vector (Array.of_list [ 1.0; 1.0; 1.0; 1.0 ])

  let ones_matrix =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ 1.0; 1.0; 1.0 ];
           Array.of_list [ 1.0; 1.0; 1.0 ];
           Array.of_list [ 1.0; 1.0; 1.0 ];
           Array.of_list [ 1.0; 1.0; 1.0 ];
         ])

  let test_matrix_1 =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ 1.0; 2.0; 3.0 ];
           Array.of_list [ 3.0; 2.0; 1.0 ];
           Array.of_list [ 1.0; 2.0; 3.0 ];
         ])

  let test_matrix_2 =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ 4.0; 5.0; 6.0 ];
           Array.of_list [ 6.0; 5.0; 4.0 ];
           Array.of_list [ 4.0; 6.0; 5.0 ];
         ])

  let test_matrix_3 =
    T.Matrix
      (Array.of_list
         [
           Array.of_list [ 28.0; 33.0; 29.0 ];
           Array.of_list [ 28.0; 31.0; 31.0 ];
           Array.of_list [ 28.0; 33.0; 29.0 ];
         ])

  let test_matrix_4 =
    T.Matrix
      (Array.of_list
         [ Array.of_list [ 1.0; 2.0; 3.0 ]; Array.of_list [ 1.0; 2.0; 3.0 ] ])

  let test_matrix_5 =
    T.Matrix
      (Array.of_list
         [ Array.of_list [ 4.0 ]; Array.of_list [ 6.0 ]; Array.of_list [ 4.0 ] ])

  let test_matrix_6 =
    T.Matrix (Array.of_list [ Array.of_list [ 28.0 ]; Array.of_list [ 28.0 ] ])

  let test_1_shape _ =
    assert_equal (get_shape 0 0) @@ T.shape empty;
    assert_equal (get_shape 4 0) @@ T.shape zeros_vector;
    assert_equal (get_shape 4 3) @@ T.shape zeros_matrix;
    assert_equal (get_shape 0 0) @@ T.shape (T.Scalar 1.0); (* Scalar case *)
    assert_equal (get_shape 1 3) @@ T.shape (T.Matrix [| [| 1.0; 2.0; 3.0 |] |]); (* Matrix case *)
    assert_equal (get_shape 3 1) @@ T.shape (T.Matrix [| [| 1.0 |]; [| 2.0 |]; [| 3.0 |] |]); (* Matrix case *)
    let random_tensor = T.random [4; 4] in
    assert_equal (get_shape 4 4) @@ T.shape random_tensor

  let test_2_zeros _ =
    assert_equal (T.Scalar 0.0) @@ T.zeros [];
    assert_equal zeros_vector @@ T.zeros [ 4 ];
    assert_equal zeros_matrix @@ T.zeros [ 4; 3 ]

  let test_3_ones _ =
    assert_equal (T.Scalar 1.0) @@ T.ones [];
    assert_equal ones_vector @@ T.ones [ 4 ];
    assert_equal ones_matrix @@ T.ones [ 4; 3 ]

  let test_4_random _ =
    assert_equal (get_shape 0 0) @@ T.shape @@ T.random [];
    assert_equal (get_shape 2 0) @@ T.shape @@ T.random [ 2 ];
    assert_equal (get_shape 4 4) @@ T.shape @@ T.random [ 4; 4 ]

  let test_5_map _ =
    let f x = x +. 1.0 in
    assert_equal empty @@ T.map f empty;
    assert_equal ones_vector @@ T.map f zeros_vector;
    assert_equal ones_matrix @@ T.map f zeros_matrix;
    let square x = x *. x in
    assert_equal (T.Vector [| 1.0; 4.0; 9.0 |]) (T.map square (T.Vector [| 1.0; 2.0; 3.0 |]))
  let test_6_map2 _ =
    let f x y = x -. y in
    assert_equal empty @@ T.map2 f empty empty;
    assert_equal zeros_vector @@ T.map2 f ones_vector ones_vector;
    assert_equal zeros_matrix @@ T.map2 f ones_matrix ones_matrix

  let test_7_dot _ =
    let product a b =
      let res = T.dot a b in
      match res with Scalar s -> s | _ -> failwith "error"
    in
    assert_equal 0.0 @@ product zeros_vector zeros_vector;
    assert_equal 0.0 @@ product ones_vector zeros_vector;
    assert_equal 4.0 @@ product ones_vector ones_vector

  let test_8_matmul _ =
    assert_equal test_matrix_3 @@ T.matmul test_matrix_1 test_matrix_2;
    assert_equal test_matrix_6 @@ T.matmul test_matrix_4 test_matrix_5;
    let mat1 = T.Matrix [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
    let mat2 = T.Matrix [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |] in
    let result = T.matmul mat1 mat2 in
    assert_equal (T.Matrix [| [| 19.0; 22.0 |]; [| 43.0; 50.0 |] |]) result;
    (* Invalid / incompatible shapes *)
    let invalid_mat = T.Matrix [|[| 1.0 |]|] in
    assert_raises (Failure "err") (fun () -> T.matmul mat1 invalid_mat)

  let test_9_basic_equality _ =
    let mat1 = T.Matrix [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
    let mat2 = T.Matrix [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
    let mat3 = T.Matrix [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |] in
    (* check equality of equal matrix *)
    assert_equal false @@ T.equal mat1 mat2;

    (* check inequality of inequal matrix *)
    assert_equal false @@ T.equal mat1 mat3;
  
    (* check equality of differently shaped matrix *)
    let mat_diff_shape = T.Matrix [| [| 1.0 |]; [| 2.0 |] |] in
    assert_equal false @@ T.equal mat1 mat_diff_shape

  let test_10_scalar_operations _ =
    assert_equal (T.Scalar 1.0) @@ T.add (T.Scalar 0.0) (T.Scalar 1.0);
    assert_equal (T.Scalar 0.0) @@ T.sub (T.Scalar 1.0) (T.Scalar 1.0);
    assert_equal (T.Scalar 0.0) @@ T.mul (T.Scalar 0.0) (T.Scalar 1.0);
    assert_equal (T.Scalar 1.0) @@ T.div (T.Scalar 1.0) 1.0;
    assert_raises (Failure "DivisionByZero") (fun () -> T.div (T.Scalar 1.0) 0.0)

  let test_11_transpose _ =
    let mat = T.Matrix [| [| 1.0; 2.0; 3.0 |]; [| 4.0; 5.0; 6.0 |] |] in
    let transposed = T.transpose mat in
    assert_equal (T.Matrix [| [| 1.0; 4.0 |]; [| 2.0; 5.0 |]; [| 3.0; 6.0 |] |]) transposed

  let test_12_neg _ =
    let neg_vector = T.Vector [| -1.0; -2.0; -3.0; -4.0 |] in
    assert_equal neg_vector @@ T.neg (T.Vector [| 1.0; 2.0; 3.0; 4.0 |]);
    
    let neg_matrix = T.Matrix [| [| -1.0; -2.0 |]; [| -3.0; -4.0 |] |] in
    assert_equal neg_matrix @@ T.neg (T.Matrix [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |])

  let test_13_flatten _ =
    let flattened_matrix = T.flatten test_matrix_1 in
    assert_equal (T.Vector [| 1.0; 2.0; 3.0; 3.0; 2.0; 1.0; 1.0; 2.0; 3.0 |]) flattened_matrix;
    let flattened_vector = T.flatten ones_vector in
    assert_equal (T.Vector [| 1.0; 1.0; 1.0; 1.0 |]) flattened_vector

  let test_14_sum _ =
    let sum_matrix = T.sum test_matrix_1 in
    assert_equal 18.0 sum_matrix;
    let sum_vector = T.sum ones_vector in
    assert_equal 4.0 sum_vector

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
           "9 - basic equality" >:: test_9_basic_equality;
            "10 - scalar operations" >:: test_10_scalar_operations;
            "11 - transpose" >:: test_11_transpose;
            "12 - neg" >:: test_12_neg;
            "13 - flatten" >:: test_13_flatten;
            "14 - sum" >:: test_14_sum;
         ]
end

let series = "Tensor tests" >::: [ Test.series ]
let () = run_test_tt_main series
