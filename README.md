# Final Project for Functional Programming in Software Engineering (601.629) FA24

[Project Requirements](https://pl.cs.jhu.edu/fpse/assignments/project.html)

The project aims to implement **Automatic Differentiation** in OCaml.

Automatic differentiation is an algorithm used to evaluate derivatives of functions expressed as a computer program. 

(Code Checkpoint) Project Progress: 

We made some revisions to the structure of our project as it was planned when we submitted our design document. 

Our variable module is a scalar-valued implementation of basic automatic differentiation. We use a hashtable of our custom variable type to calculate out the gradient and partial derivatives of a function as is necessary. This is done through OCaml syntax, where the function is defined within OCaml itself. We do this by defining our own versions of elementary operations in the variable module. A demo of how this library can be used is located in main.ml, and can be run using the instructions below. This returns a result that is more accurate than estimates using numeric differentiation.

Our tensor module replicates the functions of variable module, but it does so using tensors. While the structure of elementary functions are implemented, getting gradients, and things like the Jacobian or Hessian matrix are still unimplemented. This is what our empty forward_differentiation and reverse_differentiation files are for. They will extend out the basic functions in our tensor module. function is implemented. Both our tensor and variable modules have associated tests written for them. 

During automatic differentiation, when building a function, the function is split into primitive functions and the variables are related to each other using a computational graph. This directed acyclic graph (DAG) is then used to calculate out things like gradients. In our case, this is implicitly formed through the use of our custom type v in the variable module. 

With the base of our library extended out to working with tensors, we will use it to build some optimization functions in our optimize module, which will demonstrate the functionality and application of our library. [We still need to build a proper visualization for our demonstration and for graphing purposes, and we need to still implement proper error checking into our library. (expand?) Our ad module will be implemented last as a wrapper for the entire library.]

## Run

```
git clone https://github.com/mxberner/Autodiff-Ocaml
cd Autodiff-Ocaml
dune build
dune exec app
```
