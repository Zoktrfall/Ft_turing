let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let stats = List.exists ((=) "--stats") args in
  let no_trace = List.exists ((=) "--no-trace") args in
  let positional = List.filter (fun a -> a <> "--stats" && a <> "--no-trace") args in
  match positional with
  | ["-h"] | ["--help"] -> Machine_printer.print_help ()
  | [jsonfile; input] ->
      (match Machine_loader.load ~json_path:jsonfile with
       | Error errs ->
          Machine_printer.print_errors errs; exit 2
       | Ok machine ->
          match Machine_loader.validate_input ~machine ~input with
          | Error errs ->
              Machine_printer.print_errors errs; exit 3
          | Ok () ->
              Machine_printer.print_machine_info machine;
              if stats then
              let (reason, st) =
                  Runner.run_with_stats
                  ~max_steps:1_000_000
                  ~trace:(not no_trace)
                  ~machine ~input ()
              in
              (match reason with
              | Runner.Blocked_no_state s -> Machine_printer.print_blocked_no_state s
              | Runner.Blocked_no_rule (s,c) -> Machine_printer.print_blocked_no_rule s c
              | Runner.Step_limit_reached n -> Machine_printer.print_step_limit n
              | Runner.Final_state _ -> ());
              Machine_printer.print_stats
                  ~input
                  ~steps:st.steps
                  ~space:st.space
                  ~min_pos:st.min_pos
                  ~max_pos:st.max_pos
              else
              match Runner.run ~max_steps:1_000_000 ~machine ~input () with
              | Ok _ -> ()
              | Error (Runner.Final_state _) -> ()
              | Error (Runner.Blocked_no_state st) ->
                  Machine_printer.print_blocked_no_state st
              | Error (Runner.Blocked_no_rule (st, c)) ->
                  Machine_printer.print_blocked_no_rule st c
              | Error (Runner.Step_limit_reached n) ->
                  Machine_printer.print_step_limit n)
  | _ -> Machine_printer.print_usage ()
