
(** [read filename] reads a list of keywords from a file and returns
 * them in a Hashtbl.t
 *)
exception Error of string

val read: string -> (string, int) Hashtbl.t
