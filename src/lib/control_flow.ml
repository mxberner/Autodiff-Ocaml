open Tensor

type variable = {
  id : int;
  value : Tensor.t; (* Tensor.t *)
  local_gradients : (variable * Tensor.t) list;
}

let create_variable id value local_gradients =
  ({ id; value; local_gradients }, id + 1)

(* let add id_counter a b =
  let value = Tensor.add a.value b.value in
  let local_gradients =
    [
      (a, Tensor.ones (Tensor.shape a.value));
      (* ∂(a + b)/∂a = 1 *)
      (b, Tensor.ones (Tensor.shape b.value));
      (* ∂(a + b)/∂b = 1 *)
    ]
  in
  create_variable id_counter value local_gradients *)

module type CF = sig
  type t
  type f = t -> int -> t * int
  (* Function takes a variable and id_counter, returns a variable and updated id_counter *)

  val if_then_else : (t -> bool) -> f -> f -> f
  val while_loop : (t -> bool) -> f -> t -> int -> t * int
  val for_loop : t -> t -> f -> int -> t * int
end

module ControlFlow : CF = struct
  type t = variable
  type f = t -> int -> t * int

  let if_then_else condition then_branch else_branch input id_counter =
    if condition input then then_branch input id_counter
    else else_branch input id_counter

  let rec while_loop condition body input id_counter =
    if condition input then
      let output, id_counter = body input id_counter in
      while_loop condition body output id_counter
    else (input, id_counter)

  let rec for_loop start stop body id_counter =
    let rec loop current acc id_counter =
      if Tensor.sum (Tensor.less current.value stop.value) = 1.0 then
        let next_value =
          Tensor.add current.value (Tensor.ones (Tensor.shape current.value))
        in
        let new_acc, id_counter = body acc id_counter in
        let next_current = { current with value = next_value } in
        loop next_current new_acc id_counter
      else (acc, id_counter)
    in
    let initial_acc =
      {
        id = start.id;
        value = Tensor.zeros (Tensor.shape start.value);
        local_gradients = [];
      }
    in
    loop start initial_acc id_counter
end
