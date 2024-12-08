module T = Tensor

type t = Tensor.t
type v = { id : int; value : t; local_gradients : (v * t) list }

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

let zero = make ~local_gradients:[] (Scalar 0.0)
let one = make ~local_gradients:[] (Scalar 1.0)

let random ?seed () =
  let () = match seed with Some s -> Random.init s | None -> () in
  make @@ Scalar (Random.float 1.0)

let neg a =
  let { rows; cols } : T.s = T.shape a.value in
  let identity = T.ones [ rows; cols ] in
  let value = T.mul (Scalar (-1.0)) a.value in
  let local_gradients = [ (a, T.neg identity) ] in
  make value ~local_gradients

let sin a =
  let value = T.sin a.value in
  let local_gradients = [ (a, T.cos a.value) ] in
  make value ~local_gradients

let cos a =
  let value = T.cos a.value in
  let local_gradients = [ (a, T.mul (T.Scalar (-1.0)) @@ T.sin a.value) ] in
  make value ~local_gradients

let tan a =
  let value = T.tan a.value in
  let s = T.map (fun x -> 1.0 /. x) (T.cos a.value) in
  let local_gradients = [ (a, T.mul s s) ] in
  make value ~local_gradients

let exp a =
  let value = T.exp a.value in
  let local_gradients = [ (a, a.value) ] in
  make value ~local_gradients

let log a =
  let value = T.log a.value in
  let local_gradients = [ (a, T.map (fun x -> 1.0 /. x) a.value) ] in
  make value ~local_gradients

let pow a exponent =
  let value = T.pow a.value exponent in
  let local_gradients =
    [ (a, T.mul (Scalar exponent) (T.pow a.value (exponent -. 1.0))) ]
  in
  make value ~local_gradients

let add a b =
  let { rows = ra; cols = ca } : T.s = T.shape a.value
  and { rows = rb; cols = cb } : T.s = T.shape b.value in
  let value = T.add a.value b.value in
  let local_gradients =
    [
      (a, T.ones [ ra; ca ]);
      (* The derivative with respect to a is 1 *)
      (b, T.ones [ rb; cb ]);
      (* The derivative with respect to b is 1 *)
    ]
  in
  make value ~local_gradients

let sub a b = add a @@ neg b

let mul a b =
  let value = T.mul a.value b.value in
  let local_gradients = [ (a, b.value); (b, a.value) ] in
  make value ~local_gradients

let gradients variable : t VariableHashtbl.t =
  let gradients : t VariableHashtbl.t = VariableHashtbl.create 50 in
  let rec compute_gradients variable path_value =
    List.iter
      (fun (child_variable, local_gradient) ->
        let value_of_path_to_child = T.mul path_value local_gradient in
        let prev_grad =
          try VariableHashtbl.find gradients child_variable
          with Not_found -> Scalar 0.0
        in
        VariableHashtbl.replace gradients child_variable
          (T.add prev_grad value_of_path_to_child);
        compute_gradients child_variable value_of_path_to_child)
      variable.local_gradients
  in
  compute_gradients variable (Scalar 1.0);
  gradients

let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( ** ) = pow

(* let ( = ) = equal *)

let find gradients a =
  try VariableHashtbl.find gradients a with Not_found -> T.Scalar 0.0
