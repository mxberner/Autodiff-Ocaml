# Final Project for Functional Programming in Software Engineering (601.629) FA24

[Project Requirements](https://pl.cs.jhu.edu/fpse/assignments/project.html)

The project implements an **Automatic Differentiation** library in OCaml.

Automatic differentiation is an algorithm used to evaluate partial derivatives of functions. Automatic differentiation is more efficient than symbolic differentiation, and more accurate than numeric differentiation. Our library can be used to define functions using our `Variable` and `Tensor` modules. One can then find gradients and various partial derivatives of these functions. 
Examples are listed below. 

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
- [x] **basic** - Runs some basic operations using tensor module. 
- [x] **simple** - Run autodifferentiation on a simple function.
- [x] **hod** - Demonstrates higher order derivatives.
- [x] **nn** - Train a single layer neural network on some random data.
- [x] **visualize** - Visualizes the dynamically built computation graph. 
- [x] **printed** - Demonstrates the underlying Hashtable. 
- [ ] **cnn** - Convolutional Neural Network.


# Under the hood

When building a function representation, the function is split into primitives defined by `Variable` module. 
This results in a directed acyclic graph (DAG) where each node contains an `id`, the actual `Tensor.t` value and its calculated gradient based on function primitive type. Additionally there is an `operation` tag that specifies if the graph entry is a function of two variables, or one variable. This is mostly useful for demo purposes and looking into the computational graph.
This graph is implicitly formed through the use of a hashtable of type v in the `Variable` module. Both the `Tensor` and `Variable` modules have accompanying tests. 

# Features

- [x] First order derivative
- [x] Vectorization
- [x] N-th order derivative
- [ ] Example
    - [x] Basic
    - [x] Simple Neural Network
    - [x] Higher Order Derivatives
    - [x] Visualizing autodiff computation graph 
    - [x] Demonstration of Hashtable
    - [ ] Convolutional Neural Network
