(* 

EXAMPLE USAGE: parts would be roughly defined in utop like so. 

let a = Tensor_Node.create 5  

create returns a tensor_node w/ the value 5, and then no left or right leaves.
This could probably work by using an option type?  

let b = Tensor_Node.create 6  

this does the same thing as above, just with a different value. 
we essentially now have Tensor Nodes A and B as floating graph points. 

let c = Tensor_Node.add a b 

The add function here returns a tensor node for c, with the leaves a and b. The value is the two *tensors* added,
which would correspond to a.value + b.value. So it's using the Tensor.add function to populate the value field. 

This basically works the same for all elementary functions. 

For the README where we explain our progress, we should say that this currently requires a user to first 
decompose all the function relations into elementary subexpressions, which isn't too nice. It also requires 
the user to be defining all of these values with our specific syntax. But for now we have this predefined so 
so that we can make our DAG (directed acyclic graph) that sets up how we use autodiff. 

Not entirely sure how to translate this into ocamlgraph using their Digraph. The node type is obviously 
our tensor node, but figuring out how to add edges and vertexes properly is a bit more complicated.

module Tensor_Node = struct 
  type 'a t_node = {value: 'a; left: 'a t_node; right: 'a t_node;}  
  (* need to make the "left" and "right" leaves optional somehow... make them option types?
  also add in a local_gradients field*)
  let create v1 t1 t2 = {value = v1; left = t1; right = t2} 
  (* This is needed for defining constants *)
  let add (t1 : 'a t_node) (t2 :'a t_node) : 'a t_node = 
    {value = Tensor.add t1.value t2.value; left = t1; right = t2;}
  (* repeat this for all elementary functions *)
end;; 

*)