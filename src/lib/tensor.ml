
(* Define tensor type, which can be a Scalar, Vector, or Matrix *)
type t = Scalar of float | Vector of float array | Matrix of float array array
[@@deriving equal]

(* Define type for representing shape of tensors (rows and columns) *)
type s = { rows : int; cols : int }

(** Returns [shape] {rows,cols} *)
let shape (tensor : t) : s =
  match tensor with
  | Scalar _ -> { rows = 0; cols = 0 }
  | Vector v -> { rows = Array.length v; cols = 0 }
  | Matrix m ->
      let rows = Array.length m in
      if rows = 0 then { rows = 0; cols = 0 }
      else { rows; cols = Array.length m.(0) }

let zeros (dims : int list) : t =
  match dims with
  | [] | [ 0 ] | [ 0; 0 ] -> Scalar 0.0
  | [ n ] -> Vector (Array.make n 0.0)
  | [ rows; cols ] -> Matrix (Array.make_matrix rows cols 0.0)
  | _ -> failwith "Invalid dimensions."

let ones (dims : int list) : t =
  match dims with
  | [] | [ 0 ] | [ 0; 0 ] -> Scalar 1.0
  | [ n ] -> Vector (Array.make n 1.0)
  | [ rows; cols ] -> Matrix (Array.make_matrix rows cols 1.0)
  | _ -> failwith "Invalid dimensions."

let random ?seed (dims : int list) : t =
  let () = match seed with Some s -> Random.init s | None -> () in
  let rand _ = Random.float 1.0 in
  match dims with
  | [] | [ 0 ] | [ 0; 0 ] -> Scalar (Random.float 1.0)
  | [ n ] -> Vector (Array.init n rand)
  | [ rows; cols ] -> Matrix (Array.init rows (fun _ -> Array.init cols rand))
  | _ -> failwith "Invalid dimensions."

let map f t =
  match t with
  | Scalar a -> Scalar (f a)
  | Vector v -> Vector (Array.map f v)
  | Matrix m -> Matrix (Array.map (Array.map f) m)

let map2 f t1 t2 =
  let shape1 = shape t1 and shape2 = shape t2 in
  if shape1 <> shape2 then failwith "err"
  else
    match (t1, t2) with
    | Scalar a, Scalar b -> Scalar (f a b)
    | a, Scalar b -> map (fun x -> f x b) a
    | Scalar b, a -> map (fun x -> f x b) a
    | Vector v1, Vector v2 ->
        Vector (Array.init (Array.length v1) (fun i -> f v1.(i) v2.(i)))
    | Matrix m1, Matrix m2 ->
        let { rows; cols } = shape1 in
        Matrix
          (Array.init rows (fun i ->
               Array.init cols (fun j -> f m1.(i).(j) m2.(i).(j))))
    | _ -> failwith "err"



(**[-----ELEMENT WISE OPERATIONS-----]*)

(* Ensure the shapes are compatible for operation *)
(*let check_shape_compatibility t1 t2 =
  match (t1, t2) with
  | (Vector v1, Vector v2) when Array.length v1 = Array.length v2 -> ()
  | _ -> raise (Failure "DimensionMismatch")*)

(**[add] element-wise.]*)
let add t1 t2 =
  map2 ( +. ) t1 t2

(**[subtract] element-wise.]*)
let sub t1 t2 =
  map2 ( -. ) t1 t2

(**[multiply] element-wise.]*)
let mul t1 t2 =
  map2 ( *. ) t1 t2

(**[divide] element-wise.]*)
let div t scalar =
  if scalar = 0.0 then failwith "DivisionByZero"
  else map (fun x -> x /. scalar) t

(**[power] element-wise.]*)
let pow t exponent = map (fun a -> a ** exponent) t

(**[logarithmic] element-wise.]*)
let log t = map log t

(**[exp] element-wise.]*)
let exp t = map exp t

(**[sine] element-wise.]*)
let sin t = map sin t

(**[cosine] element-wise.]*)
let cos t = map cos t

(**[tangent] element-wise.]*)
let tan t = map tan t

(**[-^^^-ELEMENT WISE OPERATIONS-^^^-]*)


let float_of_bool b = if b then 1.0 else 0.0

(* Sum *)
let sum t =
  match t with
  | Scalar a -> a
  | Vector v -> Array.fold_left ( +. ) 0.0 v
  | Matrix m ->
      Array.fold_left
        (fun acc row -> acc +. Array.fold_left ( +. ) 0.0 row)
        0.0 m

let equal t1 t2 =
  let shape1 = shape t1 and shape2 = shape t2 in
  if shape1 <> shape2 then
    false  (* Return false if shapes are different *)
  else
    sum (map2 (fun a b -> float_of_bool @@ Float.equal a b) t1 t2) = 1.0

(* Dot product *)
let dot t1 t2 =
  let { rows = r1; cols = c1 } = shape t1
  and { rows = r2; cols = c2 } = shape t2 in
  if not ((r1 = r2 && c1 = c2) || c1 = r2) then failwith "err"
  else
    match (t1, t2) with
    | Vector _, Vector _ -> Scalar (sum @@ mul t1 t2)
    | _ -> failwith "Dot product is only defined for vectors or matrices."

(* Matrix product *)
let matmul t1 t2 =
  let { rows = r1; cols = c1 } = shape t1
  and { rows = r2; cols = c2 } = shape t2 in
  if c1 <> r2 then failwith "err" (* Matrix dimensions do not match *)
  else
    match (t1, t2) with
    | Matrix m1, Matrix m2 ->
        Matrix
          (Array.init r1 (fun i ->
               Array.init c2 (fun j ->
                   sum
                     (Vector (Array.init c1 (fun k -> m1.(i).(k) *. m2.(k).(j)))))))
    | _ -> failwith "Dot product is only defined for vectors or matrices."



(* Reshape (not supported) *)
let reshape _ _ =
  failwith "Reshape is not supported for this tensor representation."

(* Transpose *)
let transpose t =
  match t with
  | Matrix m ->
      let rows = Array.length m and cols = Array.length m.(0) in
      Matrix (Array.init cols (fun i -> Array.init rows (fun j -> m.(j).(i))))
  | _ -> failwith "err."

(* Negate *)
let neg t = map (fun x -> -1.0 *. x) t

(* Flatten *)
let flatten t =
  match t with
  | Scalar a -> Vector [| a |]
  | Vector v -> Vector v
  | Matrix m ->
      Vector (Array.concat (Array.fold_left (fun acc x -> x :: acc) [] m))

(* Operator overloading *)
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div
let ( = ) = equal
