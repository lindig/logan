
let finally (f: unit -> 'a) (free: unit -> 'b) =
  let res = try f () with exn -> free (); raise exn in
  free ();
  res


