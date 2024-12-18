open Core
open Tensor

type v = { id : int; data : t; local_gradients : (v * (v -> v)) list }

module VariableHashtbl = Hashtbl.Make (struct
  type t = v

  let hash v = Hashtbl.hash v.id
  let compare v1 v2 = Int.compare v1.id v2.id
  let t_of_sexp _ = failwith "Sexp conversion not supported"
  let sexp_of_t _ = failwith "Sexp conversion not supported"
end)

let variable_counter : int ref = ref 0

let make ?(local_gradients : (v * (v -> v)) list = []) (data : t) =
  let id = !variable_counter in
  variable_counter := Int.( + ) !variable_counter 1;
  { id; data; local_gradients }

(* Scalar constructors *)
let const f = make (zeros [||] |> map (fun _ -> f))
let zero () = const 0.0
let one () = const 1.0
let random ?seed dims = make (random ?seed dims)

(* Tensor-aware operations *)
let add (x : v) (y : v) =
  let data = x.data + y.data in
  let local_gradients =
    [
      (x, fun path_value -> path_value);
      (* d/dx (x + y) = 1 *)
      (y, fun path_value -> path_value);
      (* d/dy (x + y) = 1 *)
    ]
  in
  make data ~local_gradients

let rec mul (x : v) (y : v) =
  let data = x.data * y.data in
  let local_gradients =
    [
      (x, fun path_value -> mul path_value y);
      (* d/dx (x * y) = y *)
      (y, fun path_value -> mul path_value x);
      (* d/dy (x * y) = x *)
    ]
  in
  make data ~local_gradients

let rec div (x : v) (y : v) =
  let data = map2 ( /. ) x.data y.data in
  let local_gradients =
    [
      (x, fun path_value -> mul path_value @@ div (one ()) y);
      (* d/dx (x / y) = 1/y *)
      ( y,
        fun path_value ->
          mul path_value @@ mul (const (-1.0)) @@ div x @@ mul y y )
      (* d/dy (x / y) = -x/(y*y) *);
    ]
  in
  make data ~local_gradients

let rec neg (x : v) =
  let data = map (fun v -> -1.0 *. v) x.data in
  let local_gradients =
    [ (x, fun path_value -> mul path_value @@ div x @@ neg x) ]
  in
  make data ~local_gradients

let sub (x : v) (y : v) = add x @@ neg y

(* Exponential and logarithmic operations *)
let inv (x : v) =
  let data = map (fun v -> 1.0 /. v) x.data in
  let local_gradients =
    [
      ( x,
        fun path_value ->
          mul path_value @@ mul (const (-1.0)) @@ div (one ()) @@ mul x x );
    ]
  in
  make data ~local_gradients

let log (x : v) =
  let data = log x.data in
  let local_gradients = [ (x, fun path_value -> mul path_value @@ inv x) ] in
  make data ~local_gradients

let pow (x : v) (exp : float) =
  let data = pow x.data exp in
  let local_gradients =
    [
      ( x,
        fun path_value ->
          mul path_value @@ mul (const exp) @@ make (pow x.data (exp -. 1.0)) );
    ]
  in
  make data ~local_gradients

(* Trigonometric operations *)
let sin (x : v) =
  let data = sin x.data in
  let local_gradients =
    [ (x, fun path_value -> mul path_value @@ make (Tensor.cos x.data)) ]
  in
  make data ~local_gradients

let cos (x : v) =
  let data = cos x.data in
  let local_gradients =
    [
      ( x,
        fun path_value ->
          mul path_value @@ mul (const (-1.0)) @@ make (Tensor.sin x.data) );
    ]
  in
  make data ~local_gradients

let tan (x : v) =
  let data = tan x.data in
  let local_gradients =
    [
      ( x,
        fun path_value ->
          mul path_value
          @@ make
               (map
                  (fun e ->
                    let cs = Float.cos e in
                    1. /. (cs *. cs))
                  x.data) );
    ]
  in
  make data ~local_gradients

let exp (x : v) =
  let data = exp x.data in
  let local_gradients =
    [ (x, fun path_value -> mul path_value @@ make (exp x.data)) ]
  in
  make data ~local_gradients

(* Comparison and equality *)
let compare a b = Float.compare (get a.data [||]) (get b.data [||])
let equal a b = compare a b = 0

(* Operator overloading *)
let ( = ) = equal
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div
let ( ** ) = pow

(* Gradient computation *)
let rec compute_gradients gradients variable path_value =
  List.iter
    ~f:(fun (child_variable, f) ->
      (* Multiply edges of a path *)
      let gradient_value_of_path = f path_value in
      (* Add the different paths *)
      let prev_grad =
        match Hashtbl.find gradients child_variable with
        | Some p -> p
        | None -> zero ()
      in
      Hashtbl.set gradients ~key:child_variable
        ~data:(prev_grad + gradient_value_of_path);
      (* Recurse *)
      compute_gradients gradients child_variable gradient_value_of_path)
    variable.local_gradients

(* Gradient calculation *)
let gradients variable : v VariableHashtbl.t =
  let grad_tbl = VariableHashtbl.create () in
  compute_gradients grad_tbl variable (one ());
  grad_tbl

(* Find gradient for a specific variable *)
let find grad_tbl a =
  let f = Hashtbl.find grad_tbl a in
  match f with Some x -> x | None -> failwith "Gradient not found"

(* Printing *)
let print v =
  let dims = shape v.data in
  if Int.equal (Array.length dims) 0 then Printf.printf "%f " (get v.data [||])
  else
    Printf.printf "Tensor of shape %s\n"
      (String.concat ~sep:"x" @@ Array.to_list
      @@ Array.map ~f:string_of_int dims)
