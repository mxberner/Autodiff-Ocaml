open Core


(* Define a type for computational graph nodes *)
type node = {
  id : int; (* Unique identifier for the node *)
  label : string; (* Label for the node, e.g., "a", "b", "y" *)
  op : string option; (* Operation performed at this node, e.g., "+", "*", or None for inputs *)
  inputs : int list; (* IDs of input nodes *)
}

(* Generate a DOT representation of the computational graph *)
let generate_dot (nodes : node list) (output_file : string) =
  let open Out_channel in
  with_file output_file ~f:(fun oc ->
      (* Write the header for the DOT file *)
      output_string oc "digraph ComputationGraph {\n";
      output_string oc "  rankdir=LR;\n"; (* Left-to-right layout *)

      (* Add nodes to the DOT file *)
      List.iter nodes ~f:(fun node ->
          let shape =
            match node.op with
            | None -> "shape=circle, style=filled, color=lightblue" (* Inputs *)
            | Some _ -> "shape=ellipse, style=filled, color=lightgray"
          in
          fprintf oc "  node%d [label=\"%s\", %s];\n" node.id node.label shape);

      (* Add edges to the DOT file *)
      List.iter nodes ~f:(fun node ->
          List.iter node.inputs ~f:(fun input_id ->
              fprintf oc "  node%d -> node%d;\n" input_id node.id));

      (* Write the footer for the DOT file *)
      output_string oc "}\n")

(* Example: Generate a mock computational graph *)
let mock_graph () =
  let nodes = [
    { id = 1; label = "a"; op = None; inputs = [] }; (* Input *)
    { id = 2; label = "b"; op = None; inputs = [] }; (* Input *)
    { id = 3; label = "+"; op = Some "+"; inputs = [1; 2] }; (* a + b *)
    { id = 4; label = "y"; op = None; inputs = [3] }; (* Output *)
  ] in
  generate_dot nodes "computation_graph.dot";
  Printf.printf "Graph saved to computation_graph.dot. Use Graphviz to render it.\n"

(* Function to visualize an actual computation graph *)
let visualize_computation_graph (graph : node list) =
  let output_file = "computation_graph.dot" in
  generate_dot graph output_file;
  Printf.printf "Graph saved to %s. Use Graphviz to render it.\n" output_file

(* Call this function from main to test *)
let () =
  (* Uncomment to test the mock graph *)
  (* mock_graph () *)
  ()
