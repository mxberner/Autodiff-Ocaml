type v = { id : int; value : float; local_gradients : (v * float) list }

module VariableHashtbl = Hashtbl.Make (struct
  type t = v

  let equal v1 v2 = v1.id = v2.id
  let hash v = Hashtbl.hash v.id
end)

let variable_counter : int ref = ref 0

let make ?(local_gradients = []) value =
  let id = !variable_counter in
  variable_counter := !variable_counter + 1;
  { id; value; local_gradients }

let zero _ = make 0.0
let one _ = make 1.0

let random ?seed () =
  let () = match seed with Some s -> Random.init s | None -> () in
  make @@ Random.float 1.0

let neg a =
  let value = -1.0 *. a.value in
  let local_gradients = [ (a, -1.0) ] in
  make value ~local_gradients

let inv a =
  let value = 1. /. a.value in
  let local_gradients = [ (a, -1.0 /. (a.value ** 2.0)) ] in
  make value ~local_gradients

let sin a =
  let value = sin a.value in
  let local_gradients = [ (a, cos a.value) ] in
  make value ~local_gradients

let cos a =
  let value = cos a.value in
  let local_gradients = [ (a, -1.0 *. Float.(sin a.value)) ] in
  make value ~local_gradients

let tan a =
  let value = tan a.value in
  let sec = 1.0 /. Float.(cos a.value) in
  let local_gradients = [ (a, sec *. sec) ] in
  make value ~local_gradients

let exp a =
  let value = exp a.value in
  let local_gradients = [ (a, a.value) ] in
  make value ~local_gradients

let log a =
  let value = log a.value in
  let local_gradients = [ (a, 1.0 /. a.value) ] in
  make value ~local_gradients

let pow a exponent =
  let value = a.value ** exponent in
  let local_gradients = [ (a, exponent *. (a.value ** (exponent -. 1.0))) ] in
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
let float_of_bool b = if b then 1.0 else 0.0

let less a b =
  let value = float_of_bool (a.value < b.value) in
  let local_gradients = [ (a, b.value); (b, a.value) ] in
  make value ~local_gradients

let gradients variable : float VariableHashtbl.t =
  let gradients = VariableHashtbl.create 50 in
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

let ( < ) = less
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div
let ( ** ) = pow

let find gradients a =
  try VariableHashtbl.find gradients a with Not_found -> 0.0
