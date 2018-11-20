{ (* vim: set ts=2 sw=2 et: *)

  module L = Lexing 

  let get      = L.lexeme

  exception Error of string
  let _error fmt = Printf.kprintf (fun msg -> raise (Error msg)) fmt

  type link =
    | UUID  of string
    | ORef  of string
    | Task  of string
    | Track of string

  type line = 
    { words: string
    ; links: link list
    ; date: string option
    }
}

let digit = ['0'-'9']
let month = "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun"
          | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
let weekday = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"

let day   =     digit
          | '1' digit
          | '2' digit
          | '3' ['0'-'1']
let min   = ['0'-'5'] digit
let sec   = ['0'-'5'] digit
let hour  = ['0'-'1'] digit | '2' ['0'-'3']
let frac  = '.' digit+
let any   = [^'\n']


let ws    = [' ' '\t']
let nl    = ['\n']
let upper = ['A'-'Z']
let alpha = ['a'-'z' 'A'-'Z' '_']
let word  = alpha alpha alpha (alpha|digit)+
          | upper upper upper (alpha|digit)*

let hex   = ['0'-'9' 'a'-'f' 'A'-'F']
let hex4  = hex hex hex hex 
let hex8  = hex4 hex4 
let hex12 = hex8 hex4

let trackid = hex8 hex8 hex8 hex8

let uuid  = hex8 '-' hex4 '-' hex4 '-' hex4 '-' hex12
let oref  = "OpaqueRef:" uuid
let track = "trackid=" hex hex hex hex+
let task  = ('R'|'D') ':' hex hex hex hex+

let date  = (weekday   ' ')?
             month     ' ' ' '? 
             day       ' '
             hour      ':'      
             min       ':' 
             sec frac?

let misc    = [^ '\n' ']']+ ']'
let prefix  = date ' ' misc ' '

rule scan d words links = parse
| nl        { Some 
                { date  = d
                ; words = List.rev words |> String.concat "|" 
                ; links
                } 
            } 
| eof       { None }

| hex+      { scan d words links lexbuf }
| track     { scan d words links lexbuf }

| word      { let w = get lexbuf         in scan d (w::words)  links  lexbuf }
| uuid      { let u = UUID  (get lexbuf) in scan d  words  (u::links) lexbuf }
| oref      { let r = ORef  (get lexbuf) in scan d  words  (r::links) lexbuf }
| task      { let t = Task  (get lexbuf) in scan d  words  (t::links) lexbuf } 

| (date as date) ' ' misc
            { scan (Some date) words links lexbuf }
| _         { scan d           words links lexbuf }

{

let scan lexbuf = scan None [] [] lexbuf

let link = function
  | UUID  str -> Printf.sprintf "uuid:%s" str
  | ORef  str -> Printf.sprintf "oref:%s" str
  | Task  str -> Printf.sprintf "task:%s" str
  | Track str -> Printf.sprintf "track:%s" str

let rec dump lexbuf =
  match scan lexbuf with
  | Some { date = _; words; links } -> 
      words |> print_endline;
      List.map link links |> String.concat "|" |> print_endline;
      dump lexbuf
  | None -> ()

let read filename =
  let ic = open_in filename in
  Util.finally
    (fun () ->
      let lexbuf = Lexing.from_channel ic in
      dump lexbuf)
    (fun () ->
      close_in ic)


}
