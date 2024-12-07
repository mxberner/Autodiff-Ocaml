open Variable

(* The main data type we'll run operations on *)
type t = Scalar of v | Vector of v array | Matrix of v array array
type s = { rows : int; cols : int }

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
  | [] -> Vector (Array.make 0 zero)
  | [ n ] -> Vector (Array.make n zero)
  | [ rows; cols ] -> Matrix (Array.make_matrix rows cols zero)
  | _ -> failwith "Invalid dimensions."

let ones (dims : int list) : t =
  match dims with
  | [] -> Vector (Array.make 0 one)
  | [ n ] -> Vector (Array.make n one)
  | [ rows; cols ] -> Matrix (Array.make_matrix rows cols one)
  | _ -> failwith "Invalid dimensions."

let random ?seed (dims : int list) : t =
  let () = match seed with Some s -> Random.init s | None -> () in
  match dims with
  | [] -> Vector (Array.make 0 zero)
  | [ n ] -> Vector (Array.init n (fun _ -> random ()))
  | [ rows; cols ] ->
      Matrix (Array.init rows (fun _ -> Array.init cols (fun _ -> random ())))
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
    | Vector v1, Vector v2 ->
        Vector (Array.init (Array.length v1) (fun i -> f v1.(i) v2.(i)))
    | Matrix m1, Matrix m2 ->
        let { rows; cols } = shape1 in
        Matrix
          (Array.init rows (fun i ->
               Array.init cols (fun j -> f m1.(i).(j) m2.(i).(j))))
    | _ -> failwith "err"

(* Element-wise addition *)
let add t1 t2 = map2 ( + ) t1 t2

(* Element-wise subtraction *)
let sub t1 t2 = map2 ( - ) t1 t2

(* Element-wise multiplication *)
let mul t1 t2 = map2 ( * ) t1 t2

(* Division by scalar *)
let div t scalar =
  if scalar = make 0.0 then failwith "DivisionByZero"
  else map (fun x -> x / scalar) t

(* let equal t1 t2 = map (fun a b -> equal a b) t1 t2 *)

(* Sum *)
let sum t =
  let compute _ =
    match t with
    | Scalar a -> a
    | Vector v -> Array.fold_left ( + ) zero v
    | Matrix m ->
        Array.fold_left
          (fun acc row -> acc + Array.fold_left ( + ) zero row)
          zero m
  in
  let res = compute () in
  res.value

(* Dot product *)
let dot t1 t2 =
  let { rows = r1; cols = c1 } = shape t1
  and { rows = r2; cols = c2 } = shape t2 in
  if not ((r1 = r2 && c1 = c2) || c1 = r2) then failwith "err"
  else
    match (t1, t2) with
    | Vector _, Vector _ -> Scalar (make @@ sum @@ mul t1 t2)
    | _ -> failwith "Dot product is only defined for vectors or matrices."

(* Matrix product *)
let matmul t1 t2 =
  let { rows = r1; cols = c1 } = shape t1
  and { rows = r2; cols = c2 } = shape t2 in
  if not ((r1 = r2 && c1 = c2) || c1 = r2) then failwith "err"
  else
    match (t1, t2) with
    | Matrix m1, Matrix m2 ->
        Matrix
          (Array.init r1 (fun i ->
               Array.init c2 (fun j ->
                   make
                     (sum
                        (Vector
                           (Array.init c1 (fun k -> m1.(i).(k) * m2.(k).(j))))))))
    | _ -> failwith "Dot product is only defined for vectors or matrices."

(* Element-wise power *)
let pow t exponent = map (fun a -> a ** exponent) t
let log t = map log t
let exp t = map exp t
let sin t = map sin t
let cos t = map cos t
let tan t = map tan t

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
let neg t = map neg t

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
