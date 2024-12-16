module F = Float

type v = { id : int; value : float; local_gradients : (v * (v -> v)) list }

module VariableHashtbl = Hashtbl.Make (struct
  type t = v

  let equal v1 v2 = v1.id = v2.id
  let hash v = Hashtbl.hash v.id
end)

let variable_counter : int ref = ref 0

let make ?(local_gradients = []) value =
  let id = !variable_counter in
  variable_counter := Int.add !variable_counter 1;
  { id; value; local_gradients }

let zero () = make ~local_gradients:[] 0.0
let one () = make ~local_gradients:[] 1.0
let random () = make @@ Random.float 1.0

let neg a =
  let value = -1.0 *. a.value in
  let local_gradients =
    [
      ( a,
        fun path_value ->
          make (-.path_value.value) ~local_gradients:path_value.local_gradients
      );
    ]
  in
  make value ~local_gradients

let add a b =
  let value = F.add a.value b.value in
  let local_gradients =
    [
      (a, fun path_value -> path_value);
      (* The derivative with respect to a is 1 *)
      (b, fun path_value -> path_value);
      (* The derivative with respect to b is 1 *)
    ]
  in
  make value ~local_gradients

let rec mul a b : v =
  let value = F.mul a.value b.value in
  let local_gradients =
    [
      (a, fun path_value -> mul path_value b);
      (b, fun path_value -> mul path_value a);
    ]
  in
  make value ~local_gradients

let inv a =
  let value = 1.0 /. a.value in
  let local_gradients =
    [
      ( a,
        fun path_value ->
          mul path_value
          @@ make ~local_gradients:path_value.local_gradients
               (-1.0 /. (a.value ** 2.0)) );
    ]
  in
  make value ~local_gradients

let sub a b = add a @@ neg b
let div a b = mul a @@ inv b
let compare a b = F.compare a.value b.value
let equal a b = compare a b = 0

(* TODO: broken *)
let pow a exponent =
  let value = F.pow a.value exponent in
  let grad = exponent *. F.pow a.value (exponent -. 1.0) in
  print_float grad;
  let local_gradients =
    [
      ( a,
        fun path_value ->
          mul path_value
          @@ make grad ~local_gradients:path_value.local_gradients );
    ]
  in
  make value ~local_gradients

let ( = ) = equal
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div
let ( ** ) = pow

let sin a =
  let value = F.sin a.value in
  let local_gradients =
    [
      ( a,
        fun path_value ->
          mul path_value
          @@ make ~local_gradients:path_value.local_gradients
          @@ F.cos a.value );
    ]
  in
  make value ~local_gradients

let cos a =
  let value = F.cos a.value in
  let local_gradients =
    [
      ( a,
        fun path_value ->
          mul path_value
          @@ make ~local_gradients:path_value.local_gradients
          @@ (-1.0 *. F.sin a.value) );
    ]
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

let exp a =
  let value = F.exp a.value in
  let local_gradients = [ (a, fun path_value -> path_value) ] in
  make value ~local_gradients

let log a =
  let value = F.log a.value in
  let local_gradients = [ (a, fun path_value -> inv path_value) ] in
  make value ~local_gradients

(* Calculate the gradient of each edge *)
let gradients variable : v VariableHashtbl.t =
  let gradients : v VariableHashtbl.t = VariableHashtbl.create 50 in
  let rec compute_gradients variable path_value =
    List.iter
      (fun (child_variable, f) ->
        (* Multiply the edges of a path *)
        let value_of_path_to_child = f path_value in
        let prev_grad =
          try VariableHashtbl.find gradients child_variable
          with Not_found -> zero ()
        in
        VariableHashtbl.replace gradients child_variable
          (prev_grad + value_of_path_to_child);
        compute_gradients child_variable value_of_path_to_child)
      variable.local_gradients
  in
  compute_gradients variable (one ());
  gradients

let print_table gradients =
  VariableHashtbl.iter
    (fun x y -> Printf.printf "%f -> %f\n" x.value y.value)
    gradients

let find gradients a =
  try VariableHashtbl.find gradients a with Not_found -> make 0.0

let print v = Printf.printf "%f " v.value
