module type T = sig
  type t

  val shape : t -> int list
  val zeros : int list -> t
  val ones : int list -> t
  val random : ?seed:int -> int list -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> float -> t
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
  val sum : t -> t
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

  let shape (tensor : t) : int list =
    match tensor with
    | Scalar _ -> []
    | Vector v -> [ Array.length v ]
    | Matrix m ->
        let rows = Array.length m in
        if rows = 0 then [ 0; 0 ] else [ rows; Array.length m.(0) ]

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

  let add t1 t2 =
    match (t1, t2) with
    | Scalar a, Scalar b -> Scalar (a +. b)
    | Scalar a, Vector v | Vector v, Scalar a ->
        Vector (Array.map (fun x -> x +. a) v)
    | Scalar a, Matrix m | Matrix m, Scalar a ->
        Matrix (Array.map (Array.map (fun x -> x +. a)) m)
    | Vector v1, Vector v2 ->
        if Array.length v1 <> Array.length v2 then failwith "err"
        else Vector (Array.init (Array.length v1) (fun i -> v1.(i) +. v2.(i)))
    | Matrix m1, Matrix m2 ->
        let rows1 = Array.length m1 and rows2 = Array.length m2 in
        if rows1 <> rows2 then failwith "err"
        else
          let cols1 = if rows1 = 0 then 0 else Array.length m1.(0)
          and cols2 = if rows2 = 0 then 0 else Array.length m2.(0) in
          if cols1 <> cols2 then failwith "err"
          else
            Matrix
              (Array.init rows1 (fun i ->
                   Array.init cols1 (fun j -> m1.(i).(j) +. m2.(i).(j))))
    | _ -> failwith "err"

  (* Element-wise subtraction *)
  let sub t1 t2 =
    match (t1, t2) with
    | Scalar a, Scalar b -> Scalar (a -. b)
    | Scalar a, Vector v -> Vector (Array.map (fun x -> a -. x) v)
    | Vector v, Scalar a -> Vector (Array.map (fun x -> x -. a) v)
    | Scalar a, Matrix m -> Matrix (Array.map (Array.map (fun x -> a -. x)) m)
    | Matrix m, Scalar a -> Matrix (Array.map (Array.map (fun x -> x -. a)) m)
    | Vector v1, Vector v2 ->
        if Array.length v1 <> Array.length v2 then
          failwith "Vectors must have the same length."
        else Vector (Array.init (Array.length v1) (fun i -> v1.(i) -. v2.(i)))
    | Matrix m1, Matrix m2 ->
        let rows1 = Array.length m1 and rows2 = Array.length m2 in
        if rows1 <> rows2 then
          failwith "Matrices must have the same dimensions."
        else
          let cols1 = if rows1 = 0 then 0 else Array.length m1.(0)
          and cols2 = if rows2 = 0 then 0 else Array.length m2.(0) in
          if cols1 <> cols2 then
            failwith "Matrices must have the same dimensions."
          else
            Matrix
              (Array.init rows1 (fun i ->
                   Array.init cols1 (fun j -> m1.(i).(j) -. m2.(i).(j))))
    | _ -> failwith "Unsupported tensor types for subtraction."

  (* Element-wise multiplication *)
  let mul t1 t2 =
    match (t1, t2) with
    | Scalar a, Scalar b -> Scalar (a *. b)
    | Scalar a, Vector v | Vector v, Scalar a ->
        Vector (Array.map (fun x -> x *. a) v)
    | Scalar a, Matrix m | Matrix m, Scalar a ->
        Matrix (Array.map (Array.map (fun x -> x *. a)) m)
    | Vector v1, Vector v2 ->
        if Array.length v1 <> Array.length v2 then
          failwith "Vectors must have the same length."
        else Vector (Array.init (Array.length v1) (fun i -> v1.(i) *. v2.(i)))
    | Matrix m1, Matrix m2 ->
        let rows1 = Array.length m1 and rows2 = Array.length m2 in
        if rows1 <> rows2 then
          failwith "Matrices must have the same dimensions."
        else
          let cols1 = if rows1 = 0 then 0 else Array.length m1.(0)
          and cols2 = if rows2 = 0 then 0 else Array.length m2.(0) in
          if cols1 <> cols2 then
            failwith "Matrices must have the same dimensions."
          else
            Matrix
              (Array.init rows1 (fun i ->
                   Array.init cols1 (fun j -> m1.(i).(j) *. m2.(i).(j))))
    | _ -> failwith "Unsupported tensor types for multiplication."

  (* Division by scalar *)
  let div t scalar =
    if scalar = 0.0 then failwith "DivisionByZero";
    match t with
    | Scalar a -> Scalar (a /. scalar)
    | Vector v -> Vector (Array.map (fun x -> x /. scalar) v)
    | Matrix m -> Matrix (Array.map (Array.map (fun x -> x /. scalar)) m)

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
  let pow t exponent =
    match t with
    | Scalar a -> Scalar (a ** exponent)
    | Vector v -> Vector (Array.map (fun x -> x ** exponent) v)
    | Matrix m -> Matrix (Array.map (Array.map (fun x -> x ** exponent)) m)

  (* Element-wise mathematical functions *)
  let map_elementwise f t =
    match t with
    | Scalar a -> Scalar (f a)
    | Vector v -> Vector (Array.map f v)
    | Matrix m -> Matrix (Array.map (Array.map f) m)

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
    | _ -> failwith "Transpose is only applicable for matrices."

  (* Negate *)
  let negate t = map_elementwise (fun x -> -.x) t

  (* Flatten *)
  let flatten t =
    match t with
    | Scalar a -> Vector [| a |]
    | Vector v -> Vector v
    | Matrix m ->
        let rows = Array.length m
        and cols = if Array.length m > 0 then Array.length m.(0) else 0 in
        let flat_array = Array.make (rows * cols) 0.0 in
        for i = 0 to rows - 1 do
          Array.blit m.(i) 0 flat_array (i * cols) cols
        done;
        Vector flat_array

  (* Sum *)
  let sum t =
    match t with
    | Scalar a -> Scalar a
    | Vector v -> Scalar (Array.fold_left ( +. ) 0.0 v)
    | Matrix m ->
        Scalar
          (Array.fold_left
             (fun acc row -> acc +. Array.fold_left ( +. ) 0.0 row)
             0.0 m)

  (* Operator overloading *)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
end
