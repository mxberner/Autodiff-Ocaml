# Final Project for Functional Programming in Software Engineering (601.629) FA24

[Project Requirements](https://pl.cs.jhu.edu/fpse/assignments/project.html)

The project aims to implement **Automatic Differentiation** in OCaml.

Automatic differentiation is an algorithm used to evaluate derivatives of functions expressed as a computer program. 

(Code Checkpoint) Project Progress: 

We made some revisions to the structure of our project as it was planned when we submitted our design document. 

`Tensor` module contains the base data structure `Tensor.t` that support common scalar, vector, and matrix operations for our the automatic differentiation library.
While it supports most of the operations for current needs it can be generalized further. Additionally, it can be vectorized in the future for faster operations and to support higher order tensors. 

Our `Variable` module operates on `Tensor.t` values . It implementats basic automatic differentiation. 
We use a hashtable of our custom type `Variable.v` to store intermediate result to calculate the gradients and partial derivatives of a function.
We can define functions using standard OCaml syntax. We do this by defining our own versions of elementary operations in the `Variable` module. 
A demo of how this library can be used is located in `main.ml`, and can be run using the instructions below.

## Run

```
git clone https://github.com/mxberner/Autodiff-Ocaml
```
```
cd Autodiff-Ocaml
```
```
dune build
```
```
dune exec app
```

Under the hood when building a function representation, the function is split into primitives defined by `Variable` module. 
This results in a directed acyclic graph (DAG) where each node contains an `id`, the actual `Tensor.t` value and it's calculated gradient based on function primitive type.
This graph is implicitly formed through the use of a hashtable of type v in the `Variable` module.

With the base of our library working, the next step is to use it to build some optimization functions that are common in machine learning libraries. This will demonstrate the functionality and application of our library.

