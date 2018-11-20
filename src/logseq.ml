
module A = struct
  type t = string array
  let compare = Pervasives.compare
end

module S = Set.Make(A)

let width  = 5
let width' = width - 1

type t = 
  { mutable set: S.t
  ; window: string array
  }

let make () =
  { set = S.empty
  ; window = Array.make width ""
  }

let add t str =
  t.set <- S.add (Array.copy t.window) t.set;
  Array.blit t.window 1 t.window 0 width';
  Array.set t.window width' str 

let reset t =
  t.set <- S.add (Array.copy t.window) t.set;
  Array.fill t.window 0 width ""

let mem t str =
  Array.blit t.window 1 t.window 0 width';
  Array.set t.window width' str;
  S.mem t.window t.set

let dump t =
  S.iter t.set print_endline

let rec seq = function
  | 0 -> [0]
  | n -> n :: seq (n-1)






