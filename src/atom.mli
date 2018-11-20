(** This module tries to maximize the memory sharing between strings. It
 * stores them in an internal weak hash table.
 *)

val string: string -> string 
(** [string] acts semantically like the identity function in that it
 * returns the same string as the argument. However, the return value
 * might have a different memory location to facilitate sharing. Since
 * strings are immutable, this sharing cannot be observed except when
 * using pointer equality (==) *)
