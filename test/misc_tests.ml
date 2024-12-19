
open Bigarray
open OUnit2
open Core

module Tensor = struct
  (* Tensor operations go here *)
end

module Test = struct
  module T = Tensor

  (* Test for tensor addition *)
  let test_add _ =
    let a = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
    let b = [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |] in
    let expected = [| [| 6.0; 8.0 |]; [| 10.0; 12.0 |] |] in
    let result = T.add a b in
    assert_equal expected result ~printer:T.formatted_print
  let suite =
    "Tensor Tests"
    >::: [
           
         ]
end

let () = run_test_tt_main Test.suite
