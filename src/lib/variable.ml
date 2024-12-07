type v = { id : int; value : float; local_gradients : (v * float) list }

module VariableHashtbl = Hashtbl.Make (struct
  type t = v

  let equal v1 v2 = v1.id = v2.id
  let hash v = Hashtbl.hash v.id
end)

let variable_counter : int ref = ref 0

let create value =
  let id = !variable_counter in
  variable_counter := !variable_counter + 1;
  { id; value; local_gradients = [] }

let create_variable value local_gradients =
  let id = !variable_counter in
  variable_counter := !variable_counter + 1;
  { id; value; local_gradients }

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
  create_variable value local_gradients

let sub a b =
  let value = a.value -. b.value in
  let local_gradients =
    [
      (a, 1.0);
      (* The derivative with respect to a is 1 *)
      (b, -1.0);
      (* The derivative with respect to b is 1 *)
    ]
  in
  create_variable value local_gradients

let mul a b =
  let value = a.value *. b.value in
  let local_gradients = [ (a, b.value); (b, a.value) ] in
  create_variable value local_gradients

let div a b =
  let value = a.value *. b.value in
  let local_gradients =
    [ (a, 1.0 /. b.value); (b, -.a.value /. (b.value ** 2.0)) ]
  in
  create_variable value local_gradients

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

let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div

let find gradients a =
  try VariableHashtbl.find gradients a with Not_found -> 0.0
