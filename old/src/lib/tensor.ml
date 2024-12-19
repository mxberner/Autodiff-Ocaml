open Core
module V = Variable

type t = Scalar of V.v | Vector of V.v array | Matrix of V.v array array
type dims = { rows : int; cols : int }

exception DimensionMismatch of string

let zero _ = V.make 0.0
let one _ = V.make 1.0

let shape (tensor : t) : dims =
  match tensor with
  | Scalar _ -> { rows = 0; cols = 0 }
  | Vector v -> { rows = Array.length v; cols = 1 }
  | Matrix m ->
      let rows = Array.length m in
      if rows = 0 then { rows = 0; cols = 0 }
      else { rows; cols = Array.length m.(0) }

let zeros (dims : int list) : t =
  match dims with
  | [] | [ 0 ] | [ 0; 0 ] -> Scalar (zero ())
  | [ n ] -> Vector (Array.init n ~f:(fun _ -> zero ()))
  | [ rows; cols ] ->
      Matrix
        (Array.init rows ~f:(fun _ -> Array.init cols ~f:(fun _ -> zero ())))
  | _ -> failwith "Invalid dimensions."

let ones (dims : int list) : t =
  match dims with
  | [] | [ 0 ] | [ 0; 0 ] -> Scalar (one ())
  | [ n ] -> Vector (Array.init n ~f:(fun _ -> one ()))
  | [ rows; cols ] ->
      Matrix
        (Array.init rows ~f:(fun _ -> Array.init cols ~f:(fun _ -> one ())))
  | _ -> failwith "Invalid dimensions."

let random ?seed (dims : int list) : t =
  let () = match seed with Some s -> Random.init s | None -> () in
  match dims with
  | [] | [ 0 ] | [ 0; 0 ] -> Scalar (V.random ())
  | [ n ] -> Vector (Array.init n ~f:(fun _ -> V.random ()))
  | [ rows; cols ] ->
      Matrix
        (Array.init rows ~f:(fun _ -> Array.init cols ~f:(fun _ -> V.random ())))
  | _ -> failwith "Invalid dimensions."

let map f t =
  match t with
  | Scalar a -> Scalar (f a)
  | Vector v -> Vector (Array.map ~f v)
  | Matrix m -> Matrix (Array.map ~f:(Array.map ~f) m)

let map2 f t1 t2 =
  let { rows = r1; cols = c1 } = shape t1
  and { rows = r2; cols = c2 } = shape t2 in
  if not ((r1 = 0 && c1 = 0) || (r2 = 0 && c2 = 0) || (r1 = r2 && c1 = c2)) then
    raise (DimensionMismatch (Printf.sprintf "(%d, %d)(%d %d)" r1 c1 r2 c2))
  else
    match (t1, t2) with
    | Scalar a, Scalar b -> Scalar (f a b)
    | a, Scalar b -> map (fun x -> f x b) a
    | Scalar b, a -> map (fun x -> f x b) a
    | Vector v1, Vector v2 ->
        Vector (Array.init (Array.length v1) ~f:(fun i -> f v1.(i) v2.(i)))
    | Matrix m1, Matrix m2 ->
        Matrix
          (Array.init r1 ~f:(fun i ->
               Array.init c1 ~f:(fun j -> f m1.(i).(j) m2.(i).(j))))
    | _ -> failwith "Unable to map values"

(* Element-wise addition *)
let add t1 t2 = map2 V.add t1 t2

(* Element-wise subtraction *)
let sub t1 t2 = map2 V.sub t1 t2

(* Element-wise multiplication *)
let mul t1 t2 = map2 V.mul t1 t2

(* Sum *)
let sum t =
  match t with
  | Scalar a -> a
  | Vector v -> Array.fold ~f:V.add ~init:(V.zero ()) v
  | Matrix m ->
      Array.fold
        ~f:(fun acc v -> V.add acc @@ Array.fold ~f:V.add ~init:(V.zero ()) v)
        ~init:(V.zero ()) m

(* Dot product *)
let dot t1 t2 =
  let { rows = r1; cols = c1 } = shape t1
  and { rows = r2; cols = c2 } = shape t2 in
  if not ((r1 = r2 && c1 = c2) || c1 = r2) then
    raise (DimensionMismatch (Printf.sprintf "(%d, %d)(%d %d)" r1 c1 r2 c2))
  else
    match (t1, t2) with
    | Vector _, Vector _ -> Scalar (sum @@ mul t1 t2)
    | _ -> failwith "Dot product is only defined for vectors or matrices."

(* Matrix product *)
let matmul t1 t2 =
  let { rows = r1; cols = c1 } = shape t1
  and { rows = r2; cols = c2 } = shape t2 in
  if not ((r1 = r2 && c1 = c2) || c1 = r2) then
    raise (DimensionMismatch (Printf.sprintf "(%d, %d)(%d %d)" r1 c1 r2 c2))
  else
    match (t1, t2) with
    | Matrix m1, Matrix m2 ->
        Matrix
          (Array.init r1 ~f:(fun i ->
               Array.init c2 ~f:(fun j ->
                   sum
                     (Vector
                        (Array.init c1 ~f:(fun k -> V.mul m1.(i).(k) m2.(k).(j)))))))
    | _ -> failwith "matmul is only defined for matrices."

(* Element-wise power *)
let pow t exponent =
  match exponent with
  | Scalar s -> map (fun a -> V.pow a s) t
  | _ -> failwith "exponent can only be scalar"

let log t = map V.log t
let exp t = map V.exp t
let sin t = map V.sin t
let cos t = map V.cos t
let tan t = map V.tan t

(* Reshape (not supported) *)
let reshape _ _ =
  failwith "Reshape is not supported for this tensor representation."

(* Transpose *)
let transpose t =
  match t with
  | Scalar _ -> t
  | Vector v ->
      let rows = Array.length v and cols = 1 in
      Matrix (Array.init rows ~f:(fun i -> Array.init cols ~f:(fun _ -> v.(i))))
  | Matrix m ->
      let rows = Array.length m and cols = Array.length m.(0) in
      Matrix
        (Array.init cols ~f:(fun i -> Array.init rows ~f:(fun j -> m.(j).(i))))

(* Negate *)
let neg t = map V.neg t

(* Flatten *)
let flatten t =
  match t with
  | Scalar _ | Vector _ -> t
  | Matrix m -> Vector (Array.concat @@ Array.to_list m)

let print t =
  match t with
  | Scalar s -> V.print s
  | Vector v -> Array.iter ~f:V.print v
  | Matrix m ->
      Array.iter
        ~f:(fun v ->
          Array.iter ~f:V.print v;
          print_endline "")
        m

let float_of_bool b = if b then 1.0 else 0.0

let equal t1 t2 =
  match (t1, t2) with
  | Scalar a, Scalar b -> V.equal a b
  | _ ->
      let { rows = r1; cols = c1 } = shape t1
      and { rows = r2; cols = c2 } = shape t2 in
      if r1 <> r2 || c1 <> c2 then false
      else
        let x = max r1 1 * max c1 1 in
        let y =
          int_of_float
            (sum
               (map2 (fun x y -> V.make @@ float_of_bool (V.equal x y)) t1 t2))
              .value
        in
        x = y

(* Operator overloading *)
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( = ) = equal
