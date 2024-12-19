open Core
open Tensor

(* Variable type *)
type v = { id : int; data : t; local_gradients : (v * (t -> t)) list; operation : string }

(* Hashtable of variables used for our computational graph *)
module VariableHashtbl = Hashtbl.Make (struct
  type t = v

  let hash v = Hashtbl.hash v.id
  let compare v1 v2 = Poly.compare v1.data v2.data
  let t_of_sexp _ = failwith "Sexp conversion not supported"
  let sexp_of_t _ = failwith "Sexp conversion not supported"
end)

let variable_counter : int ref = ref 0

(* Creates the variable from a Tensor *)
let make ?(local_gradients : (v * (t -> t)) list = []) ?(operation: string = "self") (data : t) =
  let id = !variable_counter in
  variable_counter := Int.( + ) !variable_counter 1;
  { id; data; local_gradients; operation }

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
  let operation = "add" in
  make data ~local_gradients ~operation

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
  let operation = "mul" in
  make data ~local_gradients ~operation

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
  let operation = "mul" in
  make data ~local_gradients ~operation

let inv (x : v) =
  let data = map (fun v -> 1.0 /. v) x.data in
  let local_gradients =
    [ (x, fun path_value -> neg (path_value / (x.data * x.data))) ]
  in 
  let operation = "inv" in
  make data ~local_gradients ~operation

let neg (x : v) =
  let data = neg x.data in
  let local_gradients =
    [ (x, fun path_value -> path_value * neg (ones [||])) ]
  in 
  let operation = "neg" in
  make data ~local_gradients ~operation

let sub (x : v) (y : v) = add x @@ neg y

(* Logarithmic operation *)
let log (x : v) =
  let data = log x.data in
  let local_gradients = [ (x, fun path_value -> path_value / x.data) ] in 
  let operation = "log" in
  make data ~local_gradients ~operation

(* Exponent operation *)
let exp (x : v) =
  let data = exp x.data in
  let local_gradients = [ (x, fun path_value -> path_value * exp x.data) ] in 
  let operation = "exp" in
  make data ~local_gradients ~operation

(* Trigonometric operations *)
let sin (x : v) =
  let data = sin x.data in
  let local_gradients =
    [ (x, fun path_value -> path_value * Tensor.cos x.data) ]
  in 
  let operation = "sin" in
  make data ~local_gradients ~operation

let cos (x : v) =
  let data = cos x.data in
  let local_gradients =
    [ (x, fun path_value -> path_value * Tensor.neg (Tensor.sin x.data)) ]
  in 
  let operation = "cos" in
  make data ~local_gradients ~operation

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
  let operation = "tan" in
  make data ~local_gradients ~operation

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
let gradients (var : v) : t VariableHashtbl.t =
  let grad_tbl : t VariableHashtbl.t = VariableHashtbl.create () in
  compute grad_tbl var (ones [||]);
  grad_tbl

(* Find gradient for a specific variable *)
let find grad_tbl a =
  let f = Hashtbl.find grad_tbl a in
  match f with Some x -> x | None -> failwith "Gradient not found"

let sum ?(axis = -1) (x : v) =
  let data = sum ~axis x.data in
  let local_gradients = [ (x, fun path_value -> path_value + data) ] in 
  let operation = "sum" in
  make data ~local_gradients ~operation

let matmul (x : v) (y : v) =
  let data = matmul x.data y.data in
  let local_gradients =
    [
      (x, fun path_value -> path_value * transpose y.data);
      (y, fun path_value -> transpose x.data * path_value);
    ]
  in 
  let operation = "matmul" in
  make data ~local_gradients ~operation

let transpose (v : v) : v =
  let data = transpose v.data in
  let local_gradients = [ (v, fun path_value -> transpose path_value) ] in 
  let operation = "transpose" in
  make data ~local_gradients ~operation

(* Machine Learning Functions *)
let softmax ?(axis = -1) (v : v) : v =
  let exp_a = exp v in
  let s = sum ~axis v in
  let data = Tensor.exp v.data in
  let local_gradients = [ (v, fun _ -> exp_a.data / s.data) ] in 
  let operation = "softmax" in
  make data ~local_gradients ~operation

(* Leaky ReLU activation function *)
let leaky_relu ?(alpha = 0.01) (x : v) : v =
  let data =
    Tensor.map (fun v -> if Float.(v > 0.0) then v else alpha *. v) x.data
  in
  let local_gradients =
    [
      ( x,
        fun path_value ->
          Tensor.map2
            (fun v grad -> if Float.(v > 0.0) then grad else alpha *. grad)
            x.data path_value );
    ]
  in 
  let operation = "leaky_relu" in
  make data ~local_gradients ~operation

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
      Printf.printf "%d %f %s\n" e.id (Tensor.get e.data [||]) e.operation)

(* Sigmoid activation function *)
let sigmoid (x : v) : v =
  let one = create 1.0 in
  div one (add one @@ exp (neg x))

(* Binary cross-entropy loss *)
let binary_cross_entropy (y_true : v) (y_pred : v) : v =
  (* let epsilon = create 1e-7 in
  let one = create 1.0 in *)

  sum y_true - sum y_pred

(*Outputting computation graph*)
let visualize (var : v) (output_file : string) =
  let visited = Hashtbl.create (module Int) in
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "digraph computation_graph {\n";

  let rec visit_node (node : v) =
    if not (Hashtbl.mem visited node.id) then (
      Hashtbl.add_exn visited ~key:node.id ~data:();
      (* Add node label *)
      Buffer.add_string buffer
        (Printf.sprintf
           "  node%d [label=\"id: %d\\ndata: %.2f\"];\n" node.id node.id
           (Tensor.get node.data [||]));
      (* Add edges *)
      List.iter node.local_gradients ~f:(fun (child, _) ->
          Buffer.add_string buffer
            (Printf.sprintf "  node%d -> node%d;\n" node.id child.id);
          visit_node child))
  in

  visit_node var;
  Buffer.add_string buffer "}\n";
  Out_channel.write_all output_file ~data:(Buffer.contents buffer);
  Printf.printf "Computation graph written to %s\n" output_file
