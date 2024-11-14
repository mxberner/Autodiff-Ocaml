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
let result = ad.gradient f 5 (* derivative of f where x = 5 *)   

 

(* example 2 *) 

let g x = tan x 
let result = ad.gradient (g g) 5 (* derivative of tan (tan x) where x = 5 *) 

 

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

let result = ad.gradient taylor_sine 5 
```

## Libraries List 

ocamlgraph [https://github.com/backtracking/ocamlgraph]  

oplot [https://github.com/sanette/oplot] 

graphics [https://github.com/ocaml/graphics] 

Need a working demo for each non-standard library 


## Module type declarations 
[https://github.com/mxberner/Autodiff-Ocaml/blob/main/src/lib/main.mli](https://github.com/mxberner/Autodiff-Ocaml/blob/main/src/lib/main.mli)

## Features 

* Support for general mathematical functions 

* Implementations of common machine learning optimizers like SGD and Adam 

* Tools to visualize derivative path and control flow graph 

* Support for parallel computation 

* Ability to do nested derivative computations  

## Implementation plan  

### Week 1 (Nov 14 - Nov 20)

-   Implement basic tensor operation
-   Implement jacobian-vector products (JVPs, aka forward-mode auto differentiation)

* * * * *

### Week 2 (Nov 21 - Nov 27)

-   Implement vector-Jacobian products (VJPs, aka reverse-mode auto differentiation)
-   Implement common machine learning optimizers (e.g., gradient descent) to test out the auto-diff library

* * * * *

### Week 3 (Nov 28 - Dec 3)

-   Add support to checkpoint and restart backpropagation
-   Add support for visualization of the control flow graph
-   Testing
  
* * * * *

-   **Code checkpoint (Dec 4)**

* * * * *

### Week 4 (Dec 5 - Dec 11)

-   Add support for visualization of the derivative path
-   Generalize auto-diff to support more complex functions (e.g., functions with control flow like loops)

* * * * *

### Week 5 (Dec 12 - Dec 17)

-   Parallelize computation so we can deal with larger dimensional tensors
-   Demo web interface

* * * * *

### Project Demo (Dec 18)



 
