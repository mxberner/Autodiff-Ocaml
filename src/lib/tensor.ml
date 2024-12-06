type t = Scalar of float | Vector of float array | Matrix of float array array
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
  | [] -> Vector (Array.make 0 0.0)
  | [ n ] -> Vector (Array.make n 0.0)
  | [ rows; cols ] -> Matrix (Array.make_matrix rows cols 0.0)
  | _ -> failwith "Invalid dimensions."

let ones (dims : int list) : t =
  match dims with
  | [] -> Vector (Array.make 0 0.0)
  | [ n ] -> Vector (Array.make n 1.0)
  | [ rows; cols ] -> Matrix (Array.make_matrix rows cols 1.0)
  | _ -> failwith "Invalid dimensions."

let random ?seed (dims : int list) : t =
  let () = match seed with Some s -> Random.init s | None -> () in
  match dims with
  | [] -> Vector (Array.make 0 0.0)
  | [ n ] -> Vector (Array.init n (fun _ -> Random.float 1.0))
  | [ rows; cols ] ->
      Matrix
        (Array.init rows (fun _ -> Array.init cols (fun _ -> Random.float 1.0)))
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
let add t1 t2 = map2 (fun a b -> a +. b) t1 t2

(* Element-wise subtraction *)
let sub t1 t2 = map2 (fun a b -> a -. b) t1 t2

(* Element-wise multiplication *)
let mul t1 t2 = map2 (fun a b -> a *. b) t1 t2

(* Division by scalar *)
let div t scalar =
  if scalar = 0.0 then failwith "DivisionByZero"
  else map (fun x -> x /. scalar) t

let float_of_bool b = if b then 1.0 else 0.0
let less t1 t2 = map2 (fun a b -> float_of_bool (a < b)) t1 t2
let equal t1 t2 = map2 (fun a b -> float_of_bool (a = b)) t1 t2
(* let greater t1 t2 = map_elementwise2 t1 t2 (fun a b -> float_of_bool (a > b)) *)

(* Sum *)
let sum t =
  match t with
  | Scalar a -> a
  | Vector v -> Array.fold_left ( +. ) 0.0 v
  | Matrix m ->
      Array.fold_left
        (fun acc row -> acc +. Array.fold_left ( +. ) 0.0 row)
        0.0 m

(* Dot product *)
let dot t1 t2 =
  let { rows = r1; cols = c1 } = shape t1
  and { rows = r2; cols = c2 } = shape t2 in
  if not ((r1 = r2 && c1 = c2) || c1 = r2) then failwith "err"
  else
    match (t1, t2) with
    | Vector _, Vector _ -> Scalar (sum @@ mul t1 t2)
    | Matrix m1, Matrix m2 ->
        Matrix
          (Array.init r1 (fun i ->
               Array.init c2 (fun j ->
                   sum
                     (Vector (Array.init c1 (fun k -> m1.(i).(k) *. m2.(k).(j)))))))
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
let negate t = map (fun x -> -.x) t

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
