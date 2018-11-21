module C = Cmdliner

let with_lexbuf filename f = 
  let ic = open_in filename in
  Util.finally 
    (fun () -> 
      let lexbuf = Lexing.from_channel ic in
      f lexbuf)
    (fun () ->
      close_in ic)

let with_file filename f =
  let ic = open_in filename in
  Util.finally
    (fun () -> f ic)
    (fun () -> close_in ic)

let learn lexbuf =
  let model = Model.make () in
  let rec loop model =
    match Log.scan lexbuf with
    | Some Log.{links=[];_} -> loop model
    | Some Log.{links;words;_} -> 
        List.iter (fun id -> Model.add model id words) links; loop model
    | None -> model
  in
    loop model

let verify model ic =
  let rec loop ic =
    let line   = input_line ic in
    let lexbuf = Lexing.from_string line in
    match Log.scan lexbuf with
    | Some Log.{links=[];_} -> 
        Printf.printf "= %s\n" line; 
        loop ic
    | Some Log.{links;words;_} ->
        let f id =
          if Model.verify model id words then
            Printf.printf "+ %s\n" line
          else
            Printf.printf "! %s\n" line
        in
        List.iter f links; loop ic
    | None -> ()
  in
    try loop ic with End_of_file -> ()

let diff log1 log2 =
  let _kw   = Keyword.read "keywords.txt" in
  let model = with_lexbuf log1 learn in
    with_file log2 (verify (Model.reset model))


module Command = struct
  let help =
    [ `P "These options are common to all commands."
    ; `S "MORE HELP"
    ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
    ; `S "BUGS"
    ; `P "Check bug reports at https://github.com/lindig/hello/issues" ]

  let filename1 =
    C.Arg.(
      value 
      & pos 0 string "xensource.log"
      & info [] ~docv:"LOGFILE" ~doc:"logfile")

  let filename2 =
    C.Arg.(
      value 
      & pos 1 string "xensource.log"
      & info [] ~docv:"LOGFILE" ~doc:"logfile")


  let diff =
    let doc = "Read keywords" in
    C.Term.(const diff $ filename1 $ filename2, info "diff" ~doc ~man:help)
end

let main () =
  try
    match C.Term.eval Command.diff ~catch:false with
    | `Error _ -> exit 1
    | _ -> exit 0
  with exn ->
    Printf.eprintf "error: %s\n" (Printexc.to_string exn) ;
    exit 1

let () = if !Sys.interactive then () else main ()
