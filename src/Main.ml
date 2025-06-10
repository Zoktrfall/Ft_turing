let () =
  let argv = Array.to_list Sys.argv in
  match argv with
  | _ :: ("-h" | "--help") :: _ ->
      Printer.print_help ()
  | _ :: jsonfile :: input :: [] ->
      (* We’ll implement the main logic here later *)
      Printf.printf "Running Turing machine with file: %s and input: %s\n" jsonfile input
  | _ ->
      Printer.print_help ()
