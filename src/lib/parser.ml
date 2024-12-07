(* Define the AST for mathematical expressions *)
type expr =
  | Const of float               (* Constant values, e.g., 3.14 *)
  | Var of string                (* Variables, e.g., "x", "y" *)
  | Add of expr * expr           (* Addition *)
  | Mul of expr * expr           (* Multiplication *)
  | Pow of expr * int            (* Power, e.g., x^3 *)

(* Define the graph node for visualization *)
type graph_node = {
  id: int;
  operation: string;
  inputs: int list;
}

type graph = graph_node list

(* Token types for the lexer *)
type token =
  | TConst of float
  | TVar of string
  | TPlus
  | TMul
  | TPow
  | TEOF

(* Tokenizer: Converts input string into tokens *)
let tokenize input =
  let buffer = ref [] in
  let add_token t = buffer := t :: !buffer in
  let rec aux i =
    if i >= String.length input then List.rev (TEOF :: !buffer)
    else
      match input.[i] with
      | ' ' -> aux (i + 1)
      | '+' -> add_token TPlus; aux (i + 1)
      | '*' -> add_token TMul; aux (i + 1)
      | '^' -> add_token TPow; aux (i + 1)
      | c when Char.code c >= Char.code '0' && Char.code c <= Char.code '9' ->
          let j = ref i in
          while !j < String.length input &&
                (Char.code input.[!j] >= Char.code '0' &&
                 Char.code input.[!j] <= Char.code '9') do
            incr j
          done;
          add_token (TConst (float_of_string (String.sub input i (!j - i))));
          aux !j
      | c when Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' ->
          let j = ref i in
          while !j < String.length input &&
                (Char.code input.[!j] >= Char.code 'a' &&
                 Char.code input.[!j] <= Char.code 'z') do
            incr j
          done;
          add_token (TVar (String.sub input i (!j - i)));
          aux !j
      | _ -> failwith "Unexpected character in input"
  in
  aux 0

(* Recursive-descent parser *)
let rec parse_expr tokens =
  let rec parse_primary tokens =
    match tokens with
    | TConst v :: rest -> (Const v, rest)
    | TVar v :: rest -> (Var v, rest)
    | _ -> failwith "Unexpected token in primary expression"
  and parse_power tokens =
    let (left, rest) = parse_primary tokens in
    match rest with
    | TPow :: TConst e :: rest' -> (Pow (left, int_of_float e), rest')
    | _ -> (left, rest)
  and parse_term tokens =
    let rec aux left tokens =
      match tokens with
      | TMul :: rest ->
          let (right, rest') = parse_power rest in
          aux (Mul (left, right)) rest'
      | _ -> (left, tokens)
    in
    let (left, rest) = parse_power tokens in
    aux left rest
  and parse_addition tokens =
    let rec aux left tokens =
      match tokens with
      | TPlus :: rest ->
          let (right, rest') = parse_term rest in
          aux (Add (left, right)) rest'
      | _ -> (left, tokens)
    in
    let (left, rest) = parse_term tokens in
    aux left rest
  in
  parse_addition tokens

(* Convert AST to graph *)
let ast_to_graph ast =
  let counter = ref 0 in
  let nodes = ref [] in
  let rec aux ast =
    let node_id = !counter in
    incr counter;
    match ast with
    | Const v ->
        let node = { id = node_id; operation = string_of_float v; inputs = [] } in
        nodes := node :: !nodes;
        node_id
    | Var v ->
        let node = { id = node_id; operation = v; inputs = [] } in
        nodes := node :: !nodes;
        node_id
    | Add (l, r) ->
        let l_id = aux l in
        let r_id = aux r in
        let node = { id = node_id; operation = "Add"; inputs = [l_id; r_id] } in
        nodes := node :: !nodes;
        node_id
    | Mul (l, r) ->
        let l_id = aux l in
        let r_id = aux r in
        let node = { id = node_id; operation = "Mul"; inputs = [l_id; r_id] } in
        nodes := node :: !nodes;
        node_id
    | Pow (base, exp) ->
        let base_id = aux base in
        let node = { id = node_id; operation = "Pow " ^ string_of_int exp; inputs = [base_id] } in
        nodes := node :: !nodes;
        node_id
  in
  let root_id = aux ast in
  (!nodes, root_id)

(* Print the graphy *)
let print_graph graph =
  print_endline "Graph Representation:";
  let sorted_graph = List.sort (fun n1 n2 -> compare n1.id n2.id) graph in
  List.iter
    (fun node ->
       Printf.printf "Node %d: %s, Inputs: [%s]\n"
         node.id
         node.operation
         (String.concat ", " (List.map string_of_int node.inputs)))
    sorted_graph

(* Visualize the graph as a tree-like structure *)
let visualize_graph graph =
  let rec aux node_id depth =
    let node = List.find (fun n -> n.id = node_id) graph in
    Printf.printf "%sNode %d: %s\n" (String.make (depth * 2) ' ') node.id node.operation;
    List.iter (fun input -> aux input (depth + 1)) node.inputs
  in
  match graph with
  | [] -> print_endline "Empty graph."
  | root :: _ -> aux root.id 0

(* Main function *)
let () =
  print_endline "Enter a mathematical function (e.g., x^3 + 3*x + y):";
  let input = read_line () in
  let tokens = tokenize input in
  let (ast, _) = parse_expr tokens in
  let (graph, _) = ast_to_graph ast in
  (* Print the graph *)
  print_graph graph;
  print_endline "\nVisualized Graph:";
  visualize_graph graph
