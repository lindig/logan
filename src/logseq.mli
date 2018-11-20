
type t

val make: unit -> t
val add: t -> Log.link -> string -> t
val reset: t -> t
val verify: t -> string -> bool

