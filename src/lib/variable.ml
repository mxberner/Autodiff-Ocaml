module F = Float

type v = { id : int; value : float; local_gradients : (v * float) list }

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
  let local_gradients = [ (a, -1.0) ] in
  make value ~local_gradients

let inv a =
  let value = 1.0 /. a.value in
  let local_gradients = [ (a, -1.0 /. (a.value ** 2.0)) ] in
  make value ~local_gradients

let sin a =
  let value = F.sin a.value in
  let local_gradients = [ (a, F.cos a.value) ] in
  make value ~local_gradients

let cos a =
  let value = F.cos a.value in
  let local_gradients = [ (a, -1.0 *. Float.sin a.value) ] in
  make value ~local_gradients

let tan a =
  let value = F.tan a.value in
  let s = 1.0 /. F.cos a.value in
  let local_gradients = [ (a, s *. s) ] in
  make value ~local_gradients

let exp a =
  let value = F.exp a.value in
  let local_gradients = [ (a, a.value) ] in
  make value ~local_gradients

let log a =
  let value = F.log a.value in
  let local_gradients = [ (a, 1.0 /. a.value) ] in
  make value ~local_gradients

let pow a exponent =
  let value = F.pow a.value exponent in
  let local_gradients = [ (a, exponent *. F.pow a.value (exponent -. 1.0)) ] in
  make value ~local_gradients

let add a b =
  let value = a.value +. b.value in
  let local_gradients =
    [
      (a, 1.0);
      (* The derivative with respect to a is 1 *)
      (b, 1.0);
      (* The derivative with respect to b is 1 *)
    ]
  in
  make value ~local_gradients

let sub a b = add a @@ neg b

let mul a b =
  let value = a.value *. b.value in
  let local_gradients = [ (a, b.value); (b, a.value) ] in
  make value ~local_gradients

let div a b = mul a @@ inv b

let compare a b =
  if a.value < b.value then 1 else if a.value > b.value then -1 else 0

let equal a b = compare a b = 0

let gradients variable : float VariableHashtbl.t =
  let gradients : float VariableHashtbl.t = VariableHashtbl.create 50 in
  let rec compute_gradients variable path_value =
    List.iter
      (fun (child_variable, local_gradient) ->
        let value_of_path_to_child = path_value *. local_gradient in
        let prev_grad =
          try VariableHashtbl.find gradients child_variable
          with Not_found -> 0.0
        in
        VariableHashtbl.replace gradients child_variable
          (prev_grad +. value_of_path_to_child);
        compute_gradients child_variable value_of_path_to_child)
      variable.local_gradients
  in
  compute_gradients variable 1.0;
  gradients

let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div
let ( ** ) = pow
let ( = ) = equal

let print_table gradients =
  VariableHashtbl.iter
    (fun x y -> Printf.printf "%f -> %f\n" x.value y)
    gradients

let find gradients a =
  try VariableHashtbl.find gradients a with Not_found -> 0.0

let print v = Printf.printf "%f " v.value
