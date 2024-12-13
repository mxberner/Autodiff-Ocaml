open OUnit2

module Test = struct
  module T = Tensor
  module V = Variable

  let get_shape a b : T.dimensions = { rows = a; cols = b }
  let zero = T.Scalar (V.zero ())
  let zeros_vector = T.Vector [| V.zero (); V.zero (); V.zero (); V.zero () |]

  let zeros_matrix =
    T.Matrix
      [|
        [| V.zero (); V.zero (); V.zero () |];
        [| V.zero (); V.zero (); V.zero () |];
        [| V.zero (); V.zero (); V.zero () |];
        [| V.zero (); V.zero (); V.zero () |];
      |]

  let one = T.Scalar (V.one ())
  let ones_vector = T.Vector [| V.one (); V.one (); V.one (); V.one () |]

  let ones_matrix =
    T.Matrix
      [|
        [| V.one (); V.one (); V.one () |];
        [| V.one (); V.one (); V.one () |];
        [| V.one (); V.one (); V.one () |];
        [| V.one (); V.one (); V.one () |];
      |]

  let test_1_shape _ =
    assert_equal (get_shape 0 0) @@ T.shape one;

    (* Scalar case *)
    assert_equal (get_shape 4 1) @@ T.shape zeros_vector;

    (* Vector case *)
    assert_equal (get_shape 4 3) @@ T.shape zeros_matrix;
    assert_equal (get_shape 1 3)
    @@ T.shape (T.Matrix [| [| V.one (); V.make 2.0; V.make 3.0 |] |]);

    (* Matrix case *)
    assert_equal (get_shape 3 1)
    @@ T.shape
         (T.Matrix [| [| V.one () |]; [| V.make 2.0 |]; [| V.make 3.0 |] |]);
    (* Matrix case *)
    let random_tensor = T.random [ 4; 4 ] in
    assert_equal (get_shape 4 4) @@ T.shape random_tensor

  let test_2_zeros _ =
    assert_bool "" @@ T.equal zero @@ T.zeros [];
    assert_bool "" @@ T.equal zeros_vector @@ T.zeros [ 4 ];
    assert_bool "" @@ T.equal zeros_matrix @@ T.zeros [ 4; 3 ]

  let test_3_ones _ =
    assert_bool "" @@ T.equal one @@ T.ones [];
    assert_bool "" @@ T.equal ones_vector @@ T.ones [ 4 ];
    assert_bool "" @@ T.equal ones_matrix @@ T.ones [ 4; 3 ]

  let test_4_random _ =
    assert_equal (get_shape 0 0) @@ T.shape @@ T.random [];
    assert_equal (get_shape 2 1) @@ T.shape @@ T.random [ 2 ];
    assert_equal (get_shape 4 4) @@ T.shape @@ T.random [ 4; 4 ]

  let test_5_map _ =
    let f x = V.add x (V.one ()) in
    assert_bool "" @@ T.equal ones_vector @@ T.map f zeros_vector;
    assert_bool "" @@ T.equal ones_matrix @@ T.map f zeros_matrix;
    let square x = V.mul x x in
    assert_bool ""
    @@ T.equal
         (T.Vector [| V.one (); V.make 4.0; V.make 9.0 |])
         (T.map square (T.Vector [| V.one (); V.make 2.0; V.make 3.0 |]))

  let test_6_map2 _ =
    let f x y = V.sub x y in
    assert_bool "" @@ T.equal zeros_vector @@ T.map2 f ones_vector ones_vector;
    assert_bool "" @@ T.equal zeros_matrix @@ T.map2 f ones_matrix ones_matrix

  let test_7_dot _ =
    let product a b =
      let res = T.dot a b in
      match res with Scalar s -> s | _ -> failwith "error"
    in
    assert_bool "" @@ V.equal (V.zero ()) @@ product zeros_vector zeros_vector;
    assert_bool "" @@ V.equal (V.zero ()) @@ product ones_vector zeros_vector;
    assert_bool "" @@ V.equal (V.make 4.0) @@ product ones_vector ones_vector

  let test_8_matmul _ =
    let mat1 =
      T.Matrix
        [|
          [| V.one (); V.make 2.0; V.make 3.0 |];
          [| V.make 3.0; V.make 2.0; V.one () |];
          [| V.one (); V.make 2.0; V.make 3.0 |];
        |]
    and mat2 =
      T.Matrix
        [|
          [| V.make 4.0; V.make 5.0; V.make 6.0 |];
          [| V.make 6.0; V.make 5.0; V.make 4.0 |];
          [| V.make 4.0; V.make 6.0; V.make 5.0 |];
        |]
    in
    assert_bool ""
    @@ T.equal
         (T.Matrix
            [|
              [| V.make 28.0; V.make 33.0; V.make 29.0 |];
              [| V.make 28.0; V.make 31.0; V.make 31.0 |];
              [| V.make 28.0; V.make 33.0; V.make 29.0 |];
            |])
    @@ T.matmul mat1 mat2;
    let mat1 =
      T.Matrix
        [|
          [| V.one (); V.make 2.0; V.make 3.0 |];
          [| V.one (); V.make 2.0; V.make 3.0 |];
        |]
    in
    let mat2 =
      T.Matrix [| [| V.make 4.0 |]; [| V.make 6.0 |]; [| V.make 4.0 |] |]
    in
    assert_bool ""
    @@ T.equal (T.Matrix [| [| V.make 28.0 |]; [| V.make 28.0 |] |])
    @@ T.matmul mat1 mat2;
    let mat1 =
      T.Matrix [| [| V.one (); V.make 2.0 |]; [| V.make 3.0; V.make 4.0 |] |]
    in
    let mat2 =
      T.Matrix [| [| V.make 5.0; V.make 6.0 |]; [| V.make 7.0; V.make 8.0 |] |]
    in
    let result = T.matmul mat1 mat2 in
    assert_bool ""
    @@ T.equal
         (T.Matrix
            [| [| V.make 19.0; V.make 22.0 |]; [| V.make 43.0; V.make 50.0 |] |])
         result;
    (* Invalid / incompatible shapes *)
    let invalid_mat = T.Matrix [| [| V.one () |] |] in
    assert_raises (T.DimensionMismatch "(2, 2)(1 1)") (fun () ->
        T.matmul mat1 invalid_mat)

  let test_9_basic_equality _ =
    let mat1 =
      T.Matrix [| [| V.one (); V.make 2.0 |]; [| V.make 3.0; V.make 4.0 |] |]
    in
    let mat2 =
      T.Matrix [| [| V.one (); V.make 2.0 |]; [| V.make 3.0; V.make 4.0 |] |]
    in
    let mat3 =
      T.Matrix [| [| V.make 5.0; V.make 6.0 |]; [| V.make 7.0; V.make 8.0 |] |]
    in
    (* check equality of equal matrix *)
    assert_bool "" @@ T.equal mat1 mat2;

    (* check inequality of inequal matrix *)
    assert_equal false @@ T.equal mat1 mat3;

    (* check equality of differently shaped matrix *)
    let mat_diff_shape = T.Matrix [| [| V.one () |]; [| V.make 2.0 |] |] in
    assert_equal false @@ T.equal mat1 mat_diff_shape

  let test_10_scalar_operations _ =
    assert_bool "" @@ T.equal one @@ T.add one zero;
    assert_bool "" @@ T.equal zero @@ T.sub one one;
    assert_bool "" @@ T.equal zero @@ T.mul one zero
  (* assert_equal (T.Scalar (V.one ()) ) @@ T.div (T.Scalar (V.one ()) ) V.one () ;
     assert_raises (Failure "DivisionByZero") (fun () ->
         T.div (T.Scalar V.one () ) V.zero () ) *)

  let test_11_transpose _ =
    let mat =
      T.Matrix
        [|
          [| V.one (); V.make 2.0; V.make 3.0 |];
          [| V.make 4.0; V.make 5.0; V.make 6.0 |];
        |]
    in
    let transposed = T.transpose mat in
    assert_bool ""
    @@ T.equal
         (T.Matrix
            [|
              [| V.one (); V.make 4.0 |];
              [| V.make 2.0; V.make 5.0 |];
              [| V.make 3.0; V.make 6.0 |];
            |])
         transposed

  let test_12_neg _ =
    let neg_vector =
      T.Vector [| V.make (-1.0); V.make (-2.0); V.make (-3.0); V.make (-4.0) |]
    in
    assert_bool "" @@ T.equal neg_vector
    @@ T.neg (T.Vector [| V.one (); V.make 2.0; V.make 3.0; V.make 4.0 |]);

    let neg_matrix =
      T.Matrix
        [|
          [| V.make (-1.0); V.make (-2.0) |]; [| V.make (-3.0); V.make (-4.0) |];
        |]
    in
    assert_bool "" @@ T.equal neg_matrix
    @@ T.neg
         (T.Matrix
            [| [| V.one (); V.make 2.0 |]; [| V.make 3.0; V.make 4.0 |] |])

  let test_13_flatten _ =
    let matrix =
      T.Matrix
        [|
          [| V.one (); V.make 2.0; V.make 3.0 |];
          [| V.make 3.0; V.make 2.0; V.one () |];
          [| V.one (); V.make 2.0; V.make 3.0 |];
        |]
    in
    let flattened_matrix = T.flatten matrix in
    assert_bool ""
    @@ T.equal
         (T.Vector
            [|
              V.one ();
              V.make 2.0;
              V.make 3.0;
              V.make 3.0;
              V.make 2.0;
              V.one ();
              V.one ();
              V.make 2.0;
              V.make 3.0;
            |])
         flattened_matrix;
    let flattened_vector = T.flatten ones_vector in
    assert_bool ""
    @@ T.equal
         (T.Vector [| V.one (); V.one (); V.one (); V.one () |])
         flattened_vector

  let test_14_sum _ =
    let matrix =
      T.Matrix
        [|
          [| V.one (); V.make 2.0; V.make 3.0 |];
          [| V.make 3.0; V.make 2.0; V.one () |];
          [| V.one (); V.make 2.0; V.make 3.0 |];
        |]
    in
    let sum_matrix = T.sum matrix in
    assert_bool "" @@ V.equal (V.make 18.0) sum_matrix;
    let sum_vector = T.sum ones_vector in
    assert_bool "" @@ V.equal (V.make 4.0) sum_vector

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
