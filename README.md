[Project Requirements](https://pl.cs.jhu.edu/fpse/assignments/project.html)

Autodifferentiation in OCaml.


let x = [1, 2, 3]
let f = x * 6;;
let d = ml.grad (f f f f) 5;;