open Core
open Utils

(*
Command Line Use:
$ gradient.exe --gradient FUNC INPUT
*)

let () = 
  match Sys.get_argv () |> Array.to_list with
  | _ :: args -> begin
    match args with
    | "--gradient" :: func :: input -> failwith "unimplemented: --gradient"
    (*| "--gradient" :: func :: input ->
      let input = List.map ~f:Float.of_string input in
      Differentiation.gradient func input |> print_float
    *)
      | _ -> eprintf "Invalid arguments\n"
      