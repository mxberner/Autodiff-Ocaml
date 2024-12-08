module Visualize : sig
  val export_graph : filename:string -> unit
  (** Exports the current computational graph to a file. *)

  val show_graph : unit -> unit
  (** Displays the computational graph using an internal viewer. *)
end
