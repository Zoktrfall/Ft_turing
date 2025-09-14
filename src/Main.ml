let () =
  let argv = Array.to_list Sys.argv in
  match argv with
  | [_; ("-h" | "--help")] ->
      Machine_printer.print_help ()

  | [_; jsonfile; input] ->
      (match Machine_loader.load ~json_path:jsonfile with
       | Error errs ->
          Machine_printer.print_errors errs;
          exit 2
       | Ok machine ->
          match Machine_loader.validate_input ~machine ~input with
          | Error errs ->
              Machine_printer.print_errors errs;
              exit 3
          | Ok () ->
              Machine_printer.print_machine_info machine;
              match Runner.run ~machine ~input ~max_steps:1_000_000 () with
              | Ok _cfg -> ()
              | Error (Runner.Final_state st) -> ()
              | Error (Runner.Blocked_no_state st) ->
                  Machine_printer.print_blocked_no_state st
              | Error (Runner.Blocked_no_rule (st, c)) ->
                  Machine_printer.print_blocked_no_rule st c
              | Error (Runner.Step_limit_reached n) ->
                  Machine_printer.print_step_limit n )
  | _ ->
      Machine_printer.print_usage ()
