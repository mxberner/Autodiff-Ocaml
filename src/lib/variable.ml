open Core
open Tensor

type v = { id : int; data : t; local_gradients : (v * (t -> t)) list }

module VariableHashtbl = Hashtbl.Make (struct
  type t = v

  let hash v = Hashtbl.hash v.id
  let compare v1 v2 = Poly.compare v1.data v2.data
  let t_of_sexp _ = failwith "Sexp conversion not supported"
  let sexp_of_t _ = failwith "Sexp conversion not supported"
end)

let variable_counter : int ref = ref 0

let make ?(local_gradients : (v * (t -> t)) list = []) (data : t) =
  let id = !variable_counter in
  variable_counter := Int.( + ) !variable_counter 1;
  { id; data; local_gradients }

(* Scalar constructors *)
let const f = make (zeros [||] |> map (fun _ -> f))
let zero () = const 0.0
let one () = const 1.0
let create ?dims (value : float) = make (create ?dims value)
let random ?(seed : int option) (dims : dims) = make (random ?seed dims)
let get (v : v) (dims : dims) : float = get v.data dims

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

let mul (x : v) (y : v) =
  let data = x.data * y.data in
  let local_gradients =
    [
      (x, fun path_value -> path_value * y.data);
      (* d/dx (x * y) = y *)
      (y, fun path_value -> path_value * x.data);
      (* d/dy (x * y) = x *)
    ]
  in
  make data ~local_gradients

let div (x : v) (y : v) =
  let data = map2 ( /. ) x.data y.data in
  let local_gradients =
    [
      (x, fun path_value -> path_value / y.data);
      (* d/dx (x / y) = 1/y *)
      (y, fun path_value -> path_value * neg x.data / (y.data * y.data))
      (* d/dy (x / y) = -x/(y*y) *);
    ]
  in
  make data ~local_gradients

let inv (x : v) =
  let data = map (fun v -> 1.0 /. v) x.data in
  let local_gradients =
    [ (x, fun path_value -> neg (path_value / (x.data * x.data))) ]
  in
  make data ~local_gradients

let neg (x : v) =
  let data = neg x.data in
  let local_gradients =
    [ (x, fun path_value -> path_value * neg (ones [||])) ]
  in
  make data ~local_gradients

let sub (x : v) (y : v) = add x @@ neg y

(* Logarithmic operation *)
let log (x : v) =
  let data = log x.data in
  let local_gradients = [ (x, fun path_value -> path_value / x.data) ] in
  make data ~local_gradients

(* Exponent operation *)
let exp (x : v) =
  let data = exp x.data in
  let local_gradients = [ (x, fun path_value -> path_value * exp x.data) ] in
  make data ~local_gradients

(* Trigonometric operations *)
let sin (x : v) =
  let data = sin x.data in
  let local_gradients =
    [ (x, fun path_value -> path_value * Tensor.cos x.data) ]
  in
  make data ~local_gradients

let cos (x : v) =
  let data = cos x.data in
  let local_gradients =
    [ (x, fun path_value -> path_value * Tensor.neg (Tensor.sin x.data)) ]
  in
  make data ~local_gradients

let tan (x : v) =
  let data = tan x.data in
  let local_gradients =
    [
      ( x,
        fun path_value ->
          path_value
          * map
              (fun e ->
                let cs = Float.cos e in
                1. /. (cs *. cs))
              x.data );
    ]
  in
  make data ~local_gradients

(* Comparison and equality *)
let compare a b = Float.compare (get a [||]) (get b [||])
let equal a b = compare a b = 0

(* Gradient computation *)
let rec compute (grad_tbl : t VariableHashtbl.t) (var : v) (path_value : t) =
  List.iter
    ~f:(fun (child_variable, multipy_by_locg_f) ->
      (* Multiply edges of a path *)
      let gradient_value_of_path = multipy_by_locg_f path_value in
      (* Add the different paths *)
      let prev_grad =
        match Hashtbl.find grad_tbl child_variable with
        | Some p -> p
        | None -> Tensor.zeros [||]
      in
      Hashtbl.set grad_tbl ~key:child_variable
        ~data:(prev_grad + gradient_value_of_path);
      (* Recurse *)
      compute grad_tbl child_variable gradient_value_of_path)
    var.local_gradients

(* Gradient dictionary *)
let gradients (variable : v) : t VariableHashtbl.t =
  let grad_tbl : t VariableHashtbl.t = VariableHashtbl.create () in
  compute grad_tbl variable (ones [||]);
  grad_tbl

(* Find gradient for a specific variable *)
let find grad_tbl a =
  let f = Hashtbl.find grad_tbl a in
  match f with Some x -> x | None -> failwith "Gradient not found"

let sum ?(axis = -1) (x : v) =
  let data = sum ~axis x.data in
  let local_gradients = [ (x, fun path_value -> path_value + data) ] in
  make data ~local_gradients

let matmul (x : v) (y : v) =
  let d1 = shape x.data and d2 = shape y.data in
  let m = Array.length d1 and n = Array.length d2 in
  let a, b =
    match (m, n) with
    | 1, 1 -> (x, y)
    | 1, _ ->
        let v = get x [||] in
        (create ~dims:d2 v, y)
    | _, 1 ->
        let v = get y [||] in
        (x, create ~dims:d1 v)
    | _, _ -> (x, y)
  in
  let data = matmul a.data b.data in
  let local_gradients =
    [
      (x, fun path_value -> path_value * transpose b.data);
      (y, fun path_value -> transpose a.data * path_value);
    ]
  in
  make data ~local_gradients

let transpose (v : v) : v =
  let data = transpose v.data in
  let local_gradients = [ (v, fun path_value -> transpose path_value) ] in
  make data ~local_gradients

(* Machine Learning Functions *)
let softmax ?(axis = -1) (v : v) : v =
  let exp_a = exp v in
  let s = sum ~axis v in
  let data = Tensor.exp v.data in
  let local_gradients = [ (v, fun _ -> exp_a.data / s.data) ] in
  make data ~local_gradients

(* Operator overloading *)
let ( = ) = equal
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div

(* Printing *)
let print v =
  let dims = shape v.data in
  if Int.equal (Array.length dims) 0 then Printf.printf "%f " (get v [||])
  else
    Printf.printf "Tensor of shape %s\n"
      (String.concat ~sep:"x" @@ Array.to_list
      @@ Array.map ~f:string_of_int dims)

(* Printing *)
let print_table (grad_tbl : t VariableHashtbl.t) =
  Hashtbl.iter_keys grad_tbl ~f:(fun e ->
      Printf.printf "%d %f \n" e.id @@ Tensor.get e.data [||])


(* Sigmoid activation function *)
let sigmoid (x : v) : v =
  let one = create 1.0 in
  div one (add one @@ exp (neg x))

(* Binary cross-entropy loss *)
let binary_cross_entropy (y_true : v) (y_pred : v) : v =
  let epsilon = create 1e-7 in
  let loss_pos = neg (y_true * log (y_pred + epsilon)) in
  let loss_neg =
    neg ((create 1.0 - y_true) * log (create 1.0 - y_pred + epsilon))
  in
  sum loss_pos + sum loss_neg
