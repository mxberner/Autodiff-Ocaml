open Bigarray
open Core
module F = Float

type t = (float, float32_elt, c_layout) Genarray.t
type dims = int array

exception DimensionMismatch of string
exception OnlyVectorDotProductSupported
exception OnlyMatrixProductSupported

let get = Genarray.get
let shape = Genarray.dims

let create ?(dims : int array = [||]) (v : float) =
  let a = Genarray.create float32 c_layout dims in
  Genarray.fill a v;
  a

let zeros (dims : int array) : t =
  let a = Genarray.create float32 c_layout dims in
  Genarray.fill a 0.0;
  a

let ones (dims : int array) : t =
  let a = Genarray.create float32 c_layout dims in
  Genarray.fill a 1.0;
  a

let random ?seed (dims : int array) : t =
  (match seed with Some s -> Random.init s | None -> ());
  let a = Genarray.init float32 c_layout dims (fun _ -> Random.float 1.0) in
  a

let map (f : float -> float) (t : t) =
  let map_f i = f (Genarray.get t i) in
  let dims = shape t in
  Genarray.init float32 c_layout dims map_f

let map2 f t1 t2 =
  let d1 = shape t1 and d2 = shape t2 in
  if Poly.compare d1 d2 <> 0 then
    raise
      (DimensionMismatch
         (Printf.sprintf "(%s) and (%s)"
            (String.concat_array ~sep:", "
            @@ Array.map d1 ~f:(fun e -> string_of_int e))
            (String.concat_array ~sep:", "
            @@ Array.map d2 ~f:(fun e -> string_of_int e))))
  else
    let map_f i = f (Genarray.get t1 i) (Genarray.get t2 i) in
    Genarray.init float32 c_layout d1 map_f

(* Element-wise addition *)
let add t1 t2 = map2 ( +. ) t1 t2

(* Element-wise subtraction *)
let sub t1 t2 = map2 ( -. ) t1 t2

(* Element-wise multiplication *)
let mul t1 t2 = map2 ( *. ) t1 t2

(* Common operations *)
let log t = map F.log t
let exp t = map F.exp t
let sin t = map F.sin t
let cos t = map F.cos t
let tan t = map F.tan t
let pow t scalar = map (fun e -> e **. scalar) t

(* Negate *)
let neg t = map (fun e -> e *. -1.) t

let incr i dims =
  let l = Array.length i in
  let rec incr_jth k dims j n =
    if k.(j) + 1 < dims.(j) then (
      k.(j) <- k.(j) + 1;
      true)
    else (
      k.(j) <- 0;
      if j + 1 < n then incr_jth k dims (j + 1) n else false)
  in
  incr_jth i dims 0 l

let iter f t =
  let dims = Genarray.dims t in
  let n = Array.length dims in
  let index = Array.init n ~f:(fun _ -> 0) in
  let rec iter_ith f a i =
    f (Genarray.get a i);
    if incr i dims then iter_ith f a i
  in
  iter_ith f t index

let sum (t : t) : t =
  let dims = shape t in
  if Array.length dims = 0 then t
  else
    let total : float ref = ref 0.0 in
    iter (fun e -> total := !total +. e) t;
    create !total

let dot t1 t2 =
  let d1 = shape t1 and d2 = shape t2 in
  if Array.length d1 <> 1 || Array.length d2 <> 1 then
    raise OnlyVectorDotProductSupported
  else if d1.(0) <> d2.(0) then
    raise (DimensionMismatch (Printf.sprintf "(%d) and (%d)" d1.(0) d2.(0)))
  else sum @@ mul t1 t2

(* Matrix product *)
let matmul t1 t2 =
  let d1 = shape t1 and d2 = shape t2 in
  if Array.length d1 <> 2 || Array.length d2 <> 2 then
    raise OnlyMatrixProductSupported
  else
    let r1 = d1.(0) and c1 = d1.(1) and r2 = d2.(0) and c2 = d2.(1) in
    if c1 <> r2 then
      raise
        (DimensionMismatch (Printf.sprintf "(%d, %d) and (%d, %d)" r1 c1 r2 c2))
    else
      (* Create result matrix with dimensions r1 x c2 *)
      let result = zeros [| r1; c2 |] in
      for i = 0 to r1 - 1 do
        for j = 0 to c2 - 1 do
          let sum = ref 0.0 in
          for k = 0 to c1 - 1 do
            sum :=
              !sum +. (Genarray.get t1 [| i; k |] *. Genarray.get t2 [| k; j |])
          done;
          Genarray.set result [| i; j |] !sum
        done
      done;
      result

(* Transpose operation *)
let transpose t =
  let dims = shape t in
  match Array.length dims with
  | 0 -> t (* Scalar remains unchanged *)
  | 1 -> t (* 1D vector remains unchanged *)
  | 2 ->
      (* For 2D matrix, swap rows and columns *)
      let rows, cols = (dims.(0), dims.(1)) in
      let result = zeros [| cols; rows |] in
      for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
          Genarray.set result [| j; i |] (Genarray.get t [| i; j |])
        done
      done;
      result
  | n ->
      (* For higher dimensional tensors, swap first two dimensions *)
      let new_dims = Array.copy dims in
      new_dims.(0) <- dims.(1);
      new_dims.(1) <- dims.(0);
      let result = zeros new_dims in

      (* Create index mapping function *)
      let rec transpose_indices current_indices current_depth =
        if current_depth = n then (
          let transposed_indices = Array.copy current_indices in
          transposed_indices.(0) <- current_indices.(1);
          transposed_indices.(1) <- current_indices.(0);
          result_indices current_indices transposed_indices)
        else if current_depth < 2 then
          for i = 0 to dims.(current_depth) - 1 do
            current_indices.(current_depth) <- i;
            transpose_indices current_indices (current_depth + 1)
          done
        else
          for i = 0 to dims.(current_depth) - 1 do
            current_indices.(current_depth) <- i;
            transpose_indices current_indices (current_depth + 1)
          done
      and result_indices orig_indices transposed_indices =
        Genarray.set result transposed_indices (Genarray.get t orig_indices)
      in

      let initial_indices = Array.init n ~f:(fun _ -> 0) in
      transpose_indices initial_indices 0;
      result

(* Flatten operation *)
let flatten t =
  let dims = Genarray.dims t in
  let n = Array.length dims in
  match n with
  | 0 -> t (* Scalar remains unchanged *)
  | 1 -> t (* 1D vector remains unchanged *)
  | _ ->
      let total = Array.fold ~f:( * ) ~init:1 dims in
      let a = Genarray.create float32 c_layout [| total |] in
      let index = Array.init n ~f:(fun _ -> 0) in
      let c = ref 0 in
      let f el =
        Genarray.set a [| !c |] el;
        c := !c + 1
      in
      let rec iter_ith f a i =
        f (Genarray.get a i);
        if incr i dims then iter_ith f a i
      in
      iter_ith f t index;
      a

(* Reshape (not supported) *)
let reshape _ _ =
  failwith "Reshape is not supported for this tensor representation."

(* Print *)
let print t = iter (fun e -> Printf.printf "%f" e) t

(* Operator overloading *)
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
