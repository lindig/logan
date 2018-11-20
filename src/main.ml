module C = Cmdliner

let _read filename = 
  let kw = Keyword.read filename in
    Hashtbl.iter (fun str _ -> print_endline str) kw

let scan filename =
  Log.read filename


module Command = struct
  let help =
    [ `P "These options are common to all commands."
    ; `S "MORE HELP"
    ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
    ; `S "BUGS"
    ; `P "Check bug reports at https://github.com/lindig/hello/issues" ]

  let filename =
    C.Arg.(
      value & pos 0 string "kewords.txt"
      & info [] ~docv:"KEYWORDS"
          ~doc:"File containing keywords")

  let read =
    let doc = "Read keywords" in
    C.Term.(const scan $ filename, info "read" ~doc ~man:help)
end

let main () =
  try
    match C.Term.eval Command.read ~catch:false with
    | `Error _ -> exit 1
    | _ -> exit 0
  with exn ->
    Printf.eprintf "error: %s\n" (Printexc.to_string exn) ;
    exit 1

let () = if !Sys.interactive then () else main ()
