{ (* vim: set ts=2 sw=2 et: *)

  module L = Lexing

  exception Error of string
  let _error fmt = Printf.kprintf (fun msg -> raise (Error msg)) fmt

  type link =
    | UUID  of string
    | ORef  of string
    | Task  of string
    | Track of string

  type line =
    { words: string list
    ; links: link list
    ; date: string option
    }

  let link = function
    | UUID  str -> Printf.sprintf "uuid:%s" str
    | ORef  str -> Printf.sprintf "oref:%s" str
    | Task  str -> Printf.sprintf "task:%s" str
    | Track str -> Printf.sprintf "track:%s" str
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
          | upper upper (alpha|digit)*

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

rule scan is_kw line = parse
| nl        { line }
| eof       { line }

| hex+      { scan is_kw line lexbuf }
| track     { scan is_kw line lexbuf }

| word as w { if is_kw w then
                scan is_kw { line with words = w::line.words } lexbuf
              else
                scan is_kw line lexbuf
            }
| '"' (word as w) '"'
            { if is_kw w then
                scan is_kw { line with words = w::line.words } lexbuf
              else
                scan is_kw line lexbuf
            }
| uuid as x { let l = UUID x in
              scan is_kw { line with links = l::line.links } lexbuf }
| oref as x { let l = ORef x in 
              scan is_kw { line with links = l::line.links } lexbuf }
| task as x { let l = Task x in
              scan is_kw { line with links = l::line.links } lexbuf }

| (date as date) ' ' misc
            { scan is_kw { line with date = Some date } lexbuf }
| _         { scan is_kw line lexbuf }

{

let empty =
  { words = []
  ; links = []
  ; date  = None
  }

let any _ = true

let scan ?(is_kw=any) lexbuf =
  let line = scan is_kw empty lexbuf in
  { line with words = List.rev line.words }

}
