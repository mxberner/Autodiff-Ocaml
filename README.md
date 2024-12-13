# Final Project for Functional Programming in Software Engineering (601.629) FA24

[Project Requirements](https://pl.cs.jhu.edu/fpse/assignments/project.html)

The project aims to implement **Automatic Differentiation** in OCaml.

Automatic differentiation is an algorithm used to evaluate derivatives of functions expressed as a computer program. 

(Code Checkpoint) Project Progress: 

We made some revisions to the structure of our project, compared to how it was planned when we submitted our design document. 

`Variable` module operates on `Tensor.t` values. It implements basic automatic differentiation. 
We use a hashtable of our custom type `Variable.v` to store intermediate results to calculate the gradients and partial derivatives of a function.

We can define functions using standard OCaml syntax. We do this by defining our own versions of elementary operations in the `Variable` module. 


`Tensor` module contains the base data structure `Tensor.t` that supports common scalar, vector, and matrix operations for our automatic differentiation library.
While it supports most of the operations for current needs, it can be generalized further. Additionally, it can be vectorized in the future for faster operations and to support higher-order tensors. 



A demo of how this library can be used is located in `main.ml`, and can be run using the instructions below.

## Running Examples

Clone the repository and build the project.

```bash
git clone https://github.com/mxberner/Autodiff-Ocaml
cd Autodiff-Ocaml
dune build
```

Next there are a bunch of examples that make it easy
to see the library work in practice.

```bash
dune exec <example_name>
```

### Examples
    - [x] simple - Run autodifferentiation on a simple function
    - [ ] hod - Demonstrates higher order derivatives
    - [x] nn - Train a single layer neural network on some random data
    - [ ] cnn - Convolutional Neural Network


# Under the hood

When building a function representation, the function is split into primitives defined by `Variable` module. 
This results in a directed acyclic graph (DAG) where each node contains an `id`, the actual `Tensor.t` value and it's calculated gradient based on function primitive type.
This graph is implicitly formed through the use of a hashtable of type v in the `Variable` module. Both the `Tensor` and `Variable` modules have accompanying tests. 

With the base of our library working, the next step is to use it to build some optimization functions that are common in machine learning libraries. This will demonstrate the functionality and application of our library. 

To do so, we plan to extend our gradient calculations for Tensors in the `forward_differentiation` and `reverse_differentiation` modules, to calculate things like the Jacobian matrix and the Hessian matrix of a function. This will help with parts of the base of our `optimize` module. We also plan on creating some form of visualization for our automatic differentiation library. We plan on visualizing the DAG that the function is split into, for demonstration purposes. 

# Features

- [x] First order derivative
- [x] Vectorization
- [x] N-th order derivative
- [ ] Example
    - [x] Basic
    - [x] Simple Neural Network
    - [x] Higher Order Derivatives
    - [ ] Convolutional Neural Network
- [ ] More general control flow
- [ ] Optimizers
    - [ ] Gradient Descent
    - [ ] Stochastic Gradient Descent
    - [ ] Adam
- [ ] Plotting