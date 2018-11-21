module A = struct
  type t = string array

  let compare = Pervasives.compare
end

module S = Set.Make (A)

module Window = struct
  type t = string array

  let width = 4

  let width' = width - 1

  let make () = Array.make width ""

  let add t str = Array.blit t 1 t 0 width' ; t.(width') <- str

  let get t = Array.copy t
end

type t = 
  { mutable set: S.t
  ; seqs: (Log.link, Window.t) Hashtbl.t
  }

let make () = {set= S.empty; seqs= Hashtbl.create 300}

let add t id str =
  let window =
    try Hashtbl.find t.seqs id with Not_found ->
      let window = Window.make () in
      Hashtbl.add t.seqs id window ;
      window
  in
  let set = S.add (Window.get window) t.set in
  let () = Window.add window str in
  t.set <- set

let reset t =
  let clear _ window set = S.add (Window.get window) set in
  { set  = Hashtbl.fold clear t.seqs t.set 
  ; seqs = Hashtbl.create 300
  }

let verify t id str =
  let window =
    try Hashtbl.find t.seqs id with Not_found ->
      let window = Window.make () in
      Hashtbl.add t.seqs id window ;
      window
  in
  let () = Window.add window str in
  S.mem window t.set

let link = function
  | Log.UUID  str -> Printf.sprintf "uuid:%s" str
  | Log.ORef  str -> Printf.sprintf "oref:%s" str
  | Log.Task  str -> Printf.sprintf "task:%s" str
  | Log.Track str -> Printf.sprintf "track:%s" str

let stats t =
  Printf.printf "set size  = %d\n" (S.cardinal t.set);
  Printf.printf "sequences = %d\n" (Hashtbl.length t.seqs);
  (* Hashtbl.iter (fun k _ -> print_endline (link k)) t.seqs *)

