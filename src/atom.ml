module S = struct
  include String

  let hash = Hashtbl.hash
end

module H = Weak.Make (S)

let atoms = H.create 5000

let string str = try H.find atoms str with Not_found -> H.add atoms str ; str
