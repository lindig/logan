{ (* vim: set ts=2 sw=2 et: *)

  module L = Lexing 

  let get      = L.lexeme

  exception Error of string
  let _error fmt = Printf.kprintf (fun msg -> raise (Error msg)) fmt

  let add kw word = 
    if not (Hashtbl.mem kw word) then Hashtbl.add kw word 0

}

let ws    = [' ' '\t']
let nl    = ['\n']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_' '.']
let word  = alpha alpha alpha (alpha|digit)+

rule words kw = parse
| nl        { L.new_line lexbuf; words kw lexbuf }
| word      { add kw (get lexbuf); words kw lexbuf }
| _         { words kw lexbuf }
| eof       { kw }

{
  
let read filename =
  let kw = Hashtbl.create 128 in
  let ic = open_in filename in
  Util.finally
    (fun () ->
      let lexbuf = Lexing.from_channel ic in
      words kw lexbuf)
    (fun () ->
      close_in ic)

}
