open Core
module F = Float

type v = { id : int; value : float; local_gradients : (v * (v -> v)) list }

module VariableHashtbl = Hashtbl.Make (struct
  type t = v

  let hash v = Hashtbl.hash v.id
  let compare v1 v2 = Int.compare v1.id v2.id
  let t_of_sexp _ = failwith ""
  let sexp_of_t _ = failwith ""
end)

let variable_counter : int ref = ref 0

let make ?(local_gradients = []) value =
  let id = !variable_counter in
  variable_counter := !variable_counter + 1;
  { id; value; local_gradients }

let const f = make f ~local_gradients:[]
let zero () = make ~local_gradients:[] 0.0
let one () = make ~local_gradients:[] 1.0
let random () = make @@ Random.float 1.0

let add x y =
  let value = F.add x.value y.value in
  let local_gradients =
    [
      (x, fun path_value -> path_value);
      (* d/dx (x + y) = 1 *)
      (y, fun path_value -> path_value);
      (* d/dy (x + y) = 1 *)
    ]
  in
  make value ~local_gradients

let rec mul x y : v =
  let value = x.value *. y.value in
  let local_gradients =
    [
      (x, fun path_value -> mul path_value y);
      (* d/dx (x * y) = y *)
      (y, fun path_value -> mul path_value x);
      (* d/dy (x * y) = x *)
    ]
  in
  make value ~local_gradients

let rec div x y : v =
  let value = x.value /. y.value in
  let local_gradients =
    [
      (x, fun path_value -> mul path_value @@ div (one ()) y);
      (* d/dx (x / y) = 1/y *)
      ( y,
        fun path_value ->
          mul path_value @@ mul (make (-1.0)) @@ div x @@ mul y y );
      (* d/dy (x / y) = -x/(y*y)  *)
    ]
  in
  make value ~local_gradients

let rec neg x =
  let value = -1.0 *. x.value in
  let local_gradients =
    [ (x, fun path_value -> mul path_value @@ div x @@ neg x) ]
    (* d/dx (-x) = -1 *)
  in
  make value ~local_gradients

let sub x y = add x @@ neg y

let inv x =
  let value = 1.0 /. x.value in
  let local_gradients =
    [
      ( x,
        fun path_value ->
          mul path_value @@ mul (make (-1.0)) @@ div (one ()) @@ mul x x );
      (* d/dx (1/x) = -1/x^2 *)
    ]
  in
  make value ~local_gradients

let pow x exponent =
  let rec pow_ res e = if e > 1 then pow_ (mul res x) (e - 1) else res in
  (* TODO: Improve *)
  let value = x.value **. exponent.value in
  let local_gradients =
    [
      ( x,
        fun path_value ->
          mul path_value @@ mul exponent
          @@ pow_ x (int_of_float exponent.value - 1) );
      (* d/dx (x**exp) = (exp - 1)*x**exp *)
    ]
  in
  make value ~local_gradients

let compare a b = F.compare a.value b.value
let equal a b = compare a b = 0

let sin x =
  let value = F.sin x.value in
  let local_gradients =
    [ (x, fun p -> mul p @@ const (F.cos x.value)) (* d/dx sin(x) = cos(x) *) ]
  in
  make value ~local_gradients

let cos a =
  let value = F.cos a.value in
  let local_gradients =
    [ (a, fun path_value -> mul path_value @@ const (-.F.sin a.value)) ]
    (* d/dx cos(x) = -sin(x) *)
  in

  make value ~local_gradients

let tan a =
  let value = F.tan a.value in
  let local_gradients =
    let s = 1.0 /. F.cos a.value in
    [
      ( a,
        fun path_value ->
          mul path_value
          @@ make ~local_gradients:path_value.local_gradients
          @@ (s *. s) );
    ]
  in
  make value ~local_gradients

let exp x =
  let value = F.exp x.value in
  let local_gradients =
    [ (x, fun path_value -> mul path_value (const (F.exp x.value))) ]
  in
  make value ~local_gradients

let log x =
  let value = F.log x.value in
  let local_gradients = [ (x, fun path_value -> mul path_value @@ inv x) ] in
  make value ~local_gradients

(* Operator overloading *)
let ( = ) = equal
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div
let ( ** ) = pow

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

let print_table gradients =
  Hashtbl.iter ~f:(fun x -> Printf.printf "%d %f \n" x.id x.value) gradients

(* Calculate the gradient of each edge *)
let gradients variable : v VariableHashtbl.t =
  let grad_tbl = VariableHashtbl.create () in
  compute_gradients grad_tbl variable (one ());
  grad_tbl

let find grad_tbl a =
  let f = Hashtbl.find grad_tbl a in
  match f with Some x -> x | None -> failwith "not found"

let print v = Printf.printf "%f " v.value
