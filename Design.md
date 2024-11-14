# Design Document 

## Overview  

The project aims to implement automatic differentiation in OCaml.  

Automatic differentiation is an algorithm used to evaluate derivatives of functions expressed as a computer program.  

```ocaml
let f x = x * x 

let result = ad.grad f 5 (* derivative of f where x = 5 *) 
```

## What is Auto Differentiation?

Unlike symbolic differentiation which manipulates expressions directly, or numerical differentiation which approximates function derivatives, automatic differentiation computes derivatives to machine precision using the chain rule of calculus during program execution. 

The key benefits of automatic differentiation are accuracy, generality, and being able to deal with higher order derivatives. Automatic differentiation is highly accurate at computing the gradient of a function while also being able to handle complex functions with control flow structures like loops and conditions. 

Automatic differentiation evaluates the derivative at specific values without explicitly simplifying expressions. It works by decomposing functions into simple operations i.e., by creating a computational graph and applying the chain rule during evaluation. This makes it especially efficient for functions involving composition. 

Automatic Differentiation has two modes: 

*  **Forward mode** - The independent variable with respect to which the derivative must be calculated is fixed. It the computes the derivative of each sub-expression recursively. 
It is efficient when the number of input variables are small, and number of output variables are large.


* **Reverse mode** - The dependent variable to be differentiated is fixed and the derivative is computed with respect to each sub-expression recursively. 
It is efficient when the number of input variables are many and the number of outputs variables as small. 

 

## Mock Use  

The most common application of automatic differentiation is in machine learning libraries. Machine learning models, especially neural networks, often have thousands or millions of parameters. Symbolic differentiation, which requires generating explicit formulas, becomes highly impractical because it produces unwieldy, large expressions that are expensive to compute and store. 

Auto differentiation, in contrast, computes derivatives at specific points by decomposing functions into sequences of elementary operations (like addition and multiplication) and applying the chain rule. This means it avoids generating explicit formulas, reducing computational complexity and memory usage. 

 It is also heavily used in optimizers that rely on automatic differentiation for efficient gradient computation. 

```ocaml
(* example 1 *) 

let f x = x * x  

let result = ad.grad f 5 (* derivative of f where x = 5 *)   

 

(* example 2 *) 

let g x = tan x 

let result = ad.grad (g g) 5 (* derivative of tan (tan x) where x = 5 *) 

 

(* example advanced control flow *) 

let taylor_sine x = 

  let rec aux ans cur i = 

    if abs_float cur <= 0.001 then ans 

    else 

      let cur' = 

        -.cur *. x *. x /. float_of_int (((2 * i) + 3) * ((2 * i) + 2)) 

      in 

      aux (ans +. cur') cur' (i + 1) 

  in 

  aux x x 0 

let result = ad.grad taylor_sine 5 
```

## Libraries List 

ocamlgraph [https://github.com/backtracking/ocamlgraph]  

oplot [https://github.com/sanette/oplot] 

graphics [https://github.com/ocaml/graphics] 

Need a working demo for each non-standard library 


## Module type declarations 

```ocaml

(* The main data type we'll run operation *) 

module type Tensor = sig 

  type t  

  (** The type representing numerical values, which could be scalars, vectors, or matrices. *) 

 

  val shape : t -> int list 

  (** Returns the dimensions of the value as a list of integers. *) 

 

  val zeros : int list -> t 

  (** Creates a n x n tensor value filled with zeros, given the specified dimensions. *) 

 

  val ones : int list -> t 

  (** Creates a n x n tensor value filled with ones, given the specified dimensions. *) 

 

  val random : ?seed:int -> int list -> t 

  (** Generates a value with random entries, given the specified dimensions and an optional seed. *) 

end 

 

(* Some common errors when operating with n-dimensional tensors *) 

module type Errors = sig 

  exception DimensionMismatch of string 

  exception InvalidArgument of string 

  exception DivisionByZero 

end 

 

 

module type Op = sig 

  include Tensor 

 

  include Errors 

 

  val add : t -> t -> t 

  (** Element-wise addition of two values. Raises DimensionMismatch if shapes are incompatible. *) 

 

  val sub : t -> t -> t 

  (** Element-wise subtraction of two values. Raises DimensionMismatch if shapes are incompatible. *) 

 

  val mul : t -> t -> t 

  (** Element-wise multiplication of two values. Raises DimensionMismatch if shapes are incompatible. *) 

 

  val div : t -> float -> t 

  (** Element-wise division of two values. Raises DivisionByZero*) 

 

  val dot : t -> t -> t 

  (** Dot product of two values. For matrices, this represents matrix multiplication. *) 

 

  val pow : t -> float -> t 

  (** Raises each element of the value to the specified power. *) 

 

  val log : t -> t 

  (** Applies the natural logarithm element-wise. *) 

 

  val exp : t -> t 

  (** Applies the exponential function element-wise. *) 

 

  val sin : t -> t 

  (** Applies the sine function element-wise. *) 

 

  val cos : t -> t 

  (** Applies the cosine function element-wise. *) 

 

  val tan : t -> t 

  (** Applies the tangent function element-wise. *) 

 

  val reshape : t -> int list -> t 

  (** Reshapes the value to the specified dimensions. *) 

 

  val transpose : t -> t 

  (** Transposes the value (only applicable for matrices). *) 

 

  val negate : t -> t 

  (** Negates each element of the value. *) 

 

  val flatten : t -> t 

  (** Flattens the value into a one-dimensional array. *) 

 

  val sum : t -> t 

  (** Sums all elements of the value, returning a scalar value. *) 

 

  (* Operator overloading for custom operations on matrices *) 

  val ( + ) : t -> t -> t 

 

  val ( - ) : t -> t -> t 

   

  val ( * ) : t -> t -> t 

   

  val ( / ) : t -> t -> t 

end 

 

module type Function = sig 

  include Tensor 

 

  type f = t -> t 

  (** A function that maps values of type t to values of type t. *) 

 

  val eval : f -> t -> t 

  (** Evaluates the function at the given input value. *) 

 

  val compose : f -> f -> f 

  (** Composes two functions. *) 

 

  val map : (t -> t) -> f 

  (** Lifts a value-to-value function to operate over the function type f. *) 

end 

 

(* A way to represent the function control flow that we are trying to differentiate *) 

module type ControlFlow = sig 

  type t 

  type f = t -> t 

 

  val if_then_else : (t -> bool) -> f -> f -> f 

  (** Represents an if-then-else control flow construct. *) 

 

  val while_loop : (t -> bool) -> f -> t -> t 

  (** Represents a while loop construct. *) 

 

  val for_loop : t -> t -> f -> t 

  (** Represents a for loop construct. *) 

end 

 

 

module type Differentiation = sig 

  include Op 

  include Function with type t := t 

  include ControlFlow with type t := t  

 

  val gradient : f -> t -> t 

  (** Computes the gradient of the scalar-valued function at the given input. *) 

 

  val jacobian : f -> t -> t 

  (** Computes the Jacobian matrix of the function at the given input. *) 

 

  val hessian : f -> t -> t 

  (** Computes the Hessian matrix (second-order derivatives) of the function at the given input. *) 

 

  val element_wise_gradient : f -> t -> t 

  (** Computes the gradient of each element in the output with respect to the input. *) 

 

  val checkpoint : f -> f 

  (** Allows for the saving and restoring of computation graphs to manage memory usage. *) 

end 

 

(* We implement a reverse differentiation module because it is more efficienct for certain functions *) 

module type ReverseDifferentiation = sig 

  include Op 

  include Function with type t := t 

  include ControlFlow with type t := t  

 

  val gradient : f -> t -> t 

  (** Computes the gradient of the scalar-valued function at the given input. *) 

 

  val jacobian : f -> t -> t 

  (** Computes the Jacobian matrix of the function at the given input. *) 

 

  val hessian : f -> t -> t 

  (** Computes the Hessian matrix (second-order derivatives) of the function at the given input. *) 

 

  val element_wise_gradient : f -> t -> t 

  (** Computes the gradient of each element in the output with respect to the input. *) 

 

  val checkpoint : f -> f 

  (** Allows for the saving and restoring of computation graphs to manage memory usage. *) 

end 

 

(** The main module functor that, given a Value module, produces a Differentiation module with the associated types. *) 

module type AD = functor (V : Tensor) -> sig 

  include Differentiation with type t = V.t 

  include ReverseDifferentiation with type t = V.t 

end 

 

(* Machine Learning optimizer used to actually do backpropogation.  

It is a good way to test the automatic differentiation library*) 

module Optimize : sig 

  type t 

  type f 

  type optimizer = { 

    init : t; 

    step : t -> t; 

    stop_condition : t -> bool; 

  } 

 

  val gradient_descent : f -> optimizer -> t 

  (** Performs gradient descent optimization on the function. *) 

 

  val newton_raphson : f -> optimizer -> t 

  (** Performs Newton-Raphson optimization using the Hessian. *) 

end 

module Visualize : sig 

  val export_graph : filename:string -> unit 

  (** Exports the current computational graph to a file. *) 

 

  val show_graph : unit -> unit 

  (** Displays the computational graph using an internal viewer. *) 

end 
```
 

## Implementation plan  

### Week 1 (Nov 14 - Nov 20)

-   **Implement basic tensor operation**
-   **Implement jacobian-vector products (JVPs, aka forward-mode auto differentiation)**

* * * * *

### Week 2 (Nov 21 - Nov 27)

-   **Implement vector-Jacobian products (VJPs, aka reverse-mode auto differentiation)**
-   **Implement common machine learning optimizers (e.g., gradient descent) to test out the auto-diff library**

* * * * *

### Week 3 (Nov 28 - Dec 4)

-   **Add support to checkpoint and restart backpropagation**
-   **Add support for visualization of the control flow graph**
  
* * * * *

   **Code checkpoint (Dec 4)**

* * * * *

### Week 4 (Dec 5 - Dec 11)

-   **Add support for visualization of the derivative path**
-   **Generalize auto-diff to support more complex functions (e.g., functions with control flow like loops)**

* * * * *

### Week 5 (Dec 12 - Dec 18)

-   **Parallelize computation so we can deal with larger dimensional tensors**
-   **Demo web interface**

## Features 

* Support for general mathematical functions 

* Implementations of common machine learning optimizers like SGD and Adam 

* Tools to visualize derivative path and control flow graph 

* Support for parallel computation 

* Ability to do nested derivative computations 

 
