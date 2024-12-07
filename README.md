# Final Project for Functional Programming in Software Engineering (601.629) FA24

[Project Requirements](https://pl.cs.jhu.edu/fpse/assignments/project.html)

The project aims to implement **Automatic Differentiation** in OCaml.

Automatic differentiation is an algorithm used to evaluate derivatives of functions expressed as a computer program. 

(Code Checkpoint) Project Progress: 

    What's Working: Our Tensor and Variable modules are implemented, with tests. 

    What's Not: We have an untested parser, our methods of forward and reverse differentation aren't fully implemented, and we still don't have a product that interfaces through either a web or a command-line interface. Overall we still need to figure out the exact process of how we want to parse inputs and produce a result through our autodiff method. 

## Run

```
git clone https://github.com/mxberner/Autodiff-Ocaml
cd Autodiff-Ocaml
dune build
dune exec app
```