open Variable

let jacobian t1 t2 = add t1 t2
let hessian t1 t2 = add t1 t2
let element_wise_gradient (f : v) = f
let checkpoint v = print_float @@ Tensor.sum v.value
