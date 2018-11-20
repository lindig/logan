
module A = struct
  type t = string array
  let compare = Pervasives.compare
end

module S = Set.Make(A)

let width  = 5
let width' = width - 1

type seq = 
  { mutable set: S.t
  ; window: string array
  }

type t =
  | Recording of (Log.link, seq) Hashtbl.t
  | Verifying of seq

let make () = Recording (Hashtbl.create 100)

let add t id str =
  match t with
  | Verifying _ -> failwith "can't add in verification mode"
  | Recording seqs ->
    try
      let seq = Hashtbl.find seqs id in
      seq.set <- S.add (Array.copy seq.window) seq.set;
      Array.blit seq.window 1 seq.window 0 width';
      Array.set seq.window width' str;
      Recording seqs
    with Not_found ->
      let seq = 
        { set = S.empty
        ; window = Array.make width ""
        }
      in
      Array.set seq.window width' str;
      Hashtbl.add seqs id seq;
      Recording seqs

let reset t =
  match t with
  | Verifying _ -> failwith "in verification mode"
  | Recording seqs -> 
      let clear _ seq =
        seq.set <- S.add (Array.copy seq.window) seq.set in
      Hashtbl.iter clear seqs;
      let seq =
        { set = 
            Hashtbl.fold (fun _ seq set -> S.union set seq.set) seqs S.empty 
        ; window = Array.make width ""
        }
      in
        Verifying seq

let verify t str =
  match t with
  | Recording _ -> failwith "in recording mode"
  | Verifying seq ->
    Array.blit seq.window 1 seq.window 0 width';
    Array.set seq.window width' str;
    S.mem seq.window seq.set

let rec _seq = function
  | 0 -> [0]
  | n -> n :: _seq (n-1)




