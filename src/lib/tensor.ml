(*tensor.ml*)

module type Tensor = sig
  type t
  (** The type representing numerical values, which could be scalars, vectors, or matrices. *)

  val shape : t -> int list
  (** Returns the dimensions of the value as a list of integers. *)

  val zeros : int list -> t
  (** Creates a n x n tensor value filled with zeros, given the specified dimensions. *)

  val ones : int list -> t
  (** Creates a n x n tensor value filled with ones, given the specified dimensions. *)

  val random : ?seed:int -> int list -> t
  (** Generates a value with random entries, given the specified dimensions and an optional seed. *)
end


module TensorImpl : Tensor = struct
  type t = 
    | Vector of float array
    | Matrix of float array array

  let shape (tensor : t) : int list =
    match tensor with
    | Vector v -> [Array.length v]  (** 1D Tensor, returns [n] *)
    | Matrix m -> 
        let rows = Array.length m in
        if rows = 0 then [0; 0]
        else [rows; Array.length m.(0)]  (** 2D Tensor, returns [n.rows, n.cols]*)

  let zeros (dims : int list) : t =
    match dims with
    | [n] -> Vector (Array.make n 0.0)  
    | [rows; cols] -> Matrix (Array.make_matrix rows cols 0.0)  
    | _ -> failwith "Invalid dimensions."

  let ones (dims : int list) : t =
    match dims with
    | [n] -> Vector (Array.make n 1.0)
    | [rows; cols] -> Matrix (Array.make_matrix rows cols 1.0) 
    | _ -> failwith "Invalid dimensions."

  let random ?seed (dims : int list) : t =
    let () = match seed with
      | Some s -> Random.init s
      | None -> ()
    in
    match dims with
    | [n] -> Vector (Array.init n (fun _ -> Random.float 1.0))  
    | [rows; cols] ->
        Matrix (Array.init rows (fun _ -> Array.init cols (fun _ -> Random.float 1.0))) 
    | _ -> failwith "Invalid dimensions."
end
