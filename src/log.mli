exception Error of string

type link =
  | UUID of string
  | ORef of string
  | Task of string
  | Track of string

type line = 
  { words: string list
  ; links: link list
  ; date: string option
  }

val link : link -> string

val scan : Lexing.lexbuf -> line
