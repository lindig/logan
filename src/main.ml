module C = Cmdliner

let with_lexbuf filename f = 
  let ic = open_in filename in
  Util.finally 
    (fun () -> 
      let lexbuf = Lexing.from_channel ic in
      f lexbuf)
    (fun () ->
      close_in ic)

let learn lexbuf =
  let model = Logseq.make () in
  let rec loop model =
    match Log.scan lexbuf with
    | Some Log.{links=[];_} -> loop model
    | Some Log.{links;words;_} -> 
        List.fold_left (fun model id -> Logseq.add model id words) model links
    |> loop
    | None -> ()
  in
    loop model


let diff log1 _log2 =
  let _kw   = Keyword.read "keywords.txt" in
  let _model = with_lexbuf log1 learn in
    ()


module Command = struct
  let help =
    [ `P "These options are common to all commands."
    ; `S "MORE HELP"
    ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
    ; `S "BUGS"
    ; `P "Check bug reports at https://github.com/lindig/hello/issues" ]

  let filename =
    C.Arg.(
      value & pos 0 string "xensource.log"
      & info [] ~docv:"LOGFILE"
          ~doc:"logfile")

  let diff =
    let doc = "Read keywords" in
    C.Term.(const diff $ filename $ filename, info "diff" ~doc ~man:help)
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
