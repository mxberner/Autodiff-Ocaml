# Final Project for Functional Programming in Software Engineering (601.629) FA24

[Project Requirements](https://pl.cs.jhu.edu/fpse/assignments/project.html)

The project aims to implement **Automatic Differentiation** in OCaml.

Automatic differentiation is an algorithm used to evaluate derivatives of functions expressed as a computer program. 

(Code Checkpoint) Project Progress: 

What's done: We made some revisions to the structure of our project as it was planned when we submitted our design document. variable.mli is a scalar-valued implementation of the basics of autodiff, in which we use a hashtable to create an implicit computational graph that we use to calculate gradients. tensor.mli replicates the functions of variable.mli, but in tensor form, rather than the 1D applications of variable.mli. Currently, we haven't worked out implementing higher order functions, but any elementary function is implemented. In terms of usage, a prospective user would use our library to define a function in OCaml syntax, and then be able to calculate various partial derivatives. A demonstration in contained within main.ml, which can be run using dune exec app. Both our tensor and variable modules are also tested. 

What's Yet to be implemented: We still need to extend the gradient function in out variable module for tensors, which we plan on doing in the forward_differentiation and reverse_differentiation modules. We'll also implement functions for calculating things like the jacobian and the hessian. This will be used to build an optimizer in optimize.ml, which will demonstrate the functionaliyy of our library. We also want to properly visualize this. [We also still need to implement error checking into our library, so that it can reject invalid arguments.] [We also plan to implement ad.ml as a...] [To extend the scope of this project, we also want to build a simple web app that does...]

## Run

```
git clone https://github.com/mxberner/Autodiff-Ocaml
cd Autodiff-Ocaml
dune build
dune exec app
```
