module type T = sig
  type t
  type s = { rows : int; cols : int }

  val shape : t -> s
  val zeros : int list -> t
  val ones : int list -> t
  val random : ?seed:int -> int list -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> float -> t
  val less : t -> t -> t
  val equal : t -> t -> t
  val dot : t -> t -> t
  val pow : t -> float -> t
  val log : t -> t
  val exp : t -> t
  val sin : t -> t
  val cos : t -> t
  val tan : t -> t
  val reshape : t -> int list -> t
  val transpose : t -> t
  val negate : t -> t
  val flatten : t -> t
  val sum : t -> float
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> float -> t
end

module Tensor : T = struct
  type t =
    | Scalar of float
    | Vector of float array
    | Matrix of float array array

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
    | [ n ] -> Vector (Array.make n 0.0)
    | [ rows; cols ] -> Matrix (Array.make_matrix rows cols 0.0)
    | _ -> failwith "Invalid dimensions."

  let ones (dims : int list) : t =
    match dims with
    | [ n ] -> Vector (Array.make n 1.0)
    | [ rows; cols ] -> Matrix (Array.make_matrix rows cols 1.0)
    | _ -> failwith "Invalid dimensions."

  let random ?seed (dims : int list) : t =
    let () = match seed with Some s -> Random.init s | None -> () in
    match dims with
    | [ n ] -> Vector (Array.init n (fun _ -> Random.float 1.0))
    | [ rows; cols ] ->
        Matrix
          (Array.init rows (fun _ ->
               Array.init cols (fun _ -> Random.float 1.0)))
    | _ -> failwith "Invalid dimensions."

  let map_elementwise f t =
    match t with
    | Scalar a -> Scalar (f a)
    | Vector v -> Vector (Array.map f v)
    | Matrix m -> Matrix (Array.map (Array.map f) m)

  let map_elementwise2 t1 t2 f =
    let shape1 = shape t1 and shape2 = shape t2 in
    if shape1 <> shape2 then failwith "err"
    else
      match (t1, t2) with
      | Scalar a, Scalar b -> Scalar (f a b)
      | Vector v1, Vector v2 ->
          Vector (Array.init (Array.length v1) (fun i -> f v1.(i) v2.(i)))
      | Matrix m1, Matrix m2 -> (
          match shape1 with
          | { rows; cols } ->
              Matrix
                (Array.init rows (fun i ->
                     Array.init cols (fun j -> f m1.(i).(j) m2.(i).(j))))
          | _ -> failwith "err")
      | _ -> failwith "err"

  (* Element-wise addition *)
  let add t1 t2 = map_elementwise2 t1 t2 (fun a b -> a +. b)

  (* Element-wise subtraction *)
  let sub t1 t2 = map_elementwise2 t1 t2 (fun a b -> a -. b)

  (* Element-wise multiplication *)
  let mul t1 t2 = map_elementwise2 t1 t2 (fun a b -> a *. b)

  (* Division by scalar *)
  let div t scalar =
    if scalar = 0.0 then failwith "DivisionByZero"
    else map_elementwise (fun x -> x /. scalar) t

  let float_of_bool b = if b then 1.0 else 0.0
  let less t1 t2 = map_elementwise2 t1 t2 (fun a b -> float_of_bool (a < b))
  let equal t1 t2 = map_elementwise2 t1 t2 (fun a b -> float_of_bool (a = b))
  let greater t1 t2 = map_elementwise2 t1 t2 (fun a b -> float_of_bool (a > b))

  (* Dot product *)
  let dot t1 t2 =
    match (t1, t2) with
    | Vector v1, Vector v2 ->
        if Array.length v1 <> Array.length v2 then
          failwith "Vectors must have the same length."
        else
          let sum = ref 0.0 in
          for i = 0 to Array.length v1 - 1 do
            sum := !sum +. (v1.(i) *. v2.(i))
          done;
          Scalar !sum
    | Matrix m1, Matrix m2 ->
        let rows1 = Array.length m1 and cols1 = Array.length m1.(0) in
        let rows2 = Array.length m2 and cols2 = Array.length m2.(0) in
        if cols1 <> rows2 then
          failwith "Inner dimensions must match for matrix multiplication."
        else
          Matrix
            (Array.init rows1 (fun i ->
                 Array.init cols2 (fun j ->
                     let sum = ref 0.0 in
                     for k = 0 to cols1 - 1 do
                       sum := !sum +. (m1.(i).(k) *. m2.(k).(j))
                     done;
                     !sum)))
    | _ -> failwith "Dot product is only defined for vectors or matrices."

  (* Element-wise power *)
  let pow t exponent = map_elementwise (fun a -> a ** exponent) t
  let log t = map_elementwise log t
  let exp t = map_elementwise exp t
  let sin t = map_elementwise sin t
  let cos t = map_elementwise cos t
  let tan t = map_elementwise tan t

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
  let negate t = map_elementwise (fun x -> -.x) t

  (* Flatten *)
  let flatten t =
    match t with
    | Scalar a -> Vector [| a |]
    | Vector v -> Vector v
    | Matrix m ->
        Vector (Array.concat (Array.fold_left (fun acc x -> x :: acc) [] m))

  (* Sum *)
  let sum t =
    match t with
    | Scalar a -> a
    | Vector v -> Array.fold_left ( +. ) 0.0 v
    | Matrix m ->
        Array.fold_left
          (fun acc row -> acc +. Array.fold_left ( +. ) 0.0 row)
          0.0 m

  (* Operator overloading *)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
end
