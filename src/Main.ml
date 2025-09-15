let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let stats = List.exists ((=) "--stats") args in
  let no_trace = List.exists ((=) "--no-trace") args in
  let positional = List.filter (fun a -> a <> "--stats" && a <> "--no-trace") args in

  match positional with
  | ["-h"] | ["--help"] ->
      Machine_printer.print_help ()

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

               let max_steps = 1_000_000 in
               let trace = not no_trace in

               let print_reason = function
                 | Runner.Final_state _ -> ()
                 | Runner.Blocked_no_state s    -> Machine_printer.print_blocked_no_state s
                 | Runner.Blocked_no_rule (s,c) -> Machine_printer.print_blocked_no_rule s c
                 | Runner.Step_limit_reached n  -> Machine_printer.print_step_limit n
               in

               if stats then begin
                 let (reason, st) =
                   Runner.run_with_stats ~max_steps ~trace ~machine ~input ()
                 in
                 print_reason reason;
                 Machine_printer.print_stats
                   ~input
                   ~steps:st.steps
                   ~space:st.space
                   ~min_pos:st.min_pos
                   ~max_pos:st.max_pos
               end else begin
                 match Runner.run ~max_steps ~machine ~input () with
                 | Ok _ -> ()
                 | Error reason -> print_reason reason
               end)

  | _ ->
      Machine_printer.print_usage ()