(* variable.ml *)

let variable_counter = ref 0

type variable = {
  id : int;
  value : float;
  local_gradients : (variable * float) list;
}

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

let mul a b =
  let value = a.value *. b.value in
  let local_gradients =
    [
      (a, b.value);
      (* The derivative with respect to a is b.value *)
      (b, a.value);
      (* The derivative with respect to b is a.value *)
    ]
  in
  create_variable value local_gradients

module VariableHashtbl = Hashtbl.Make (struct
  type t = variable

  let equal v1 v2 = v1.id = v2.id
  let hash v = Hashtbl.hash v.id
end)

let get_gradients variable =
  let gradients = VariableHashtbl.create 10 in
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

(* main.ml *)

(* Assuming variable.ml is compiled and linked *)

let f a b c = mul a (add b c)

let () =
  (* Create variables *)
  let a = create_variable 4.0 [] in
  let b = create_variable 3.0 [] in
  let c = add a b in
  let d = f a b c in

  (* Print the value of d *)
  Printf.printf "d.value = %f\n" d.value;

  (* Compute gradients *)
  let gradients = get_gradients d in

  (* Get the gradient of d with respect to a *)
  let grad_a = try VariableHashtbl.find gradients a with Not_found -> 0.0 in

  (* Print the partial derivative *)
  Printf.printf "The partial derivative of d with respect to a = %f\n" grad_a
