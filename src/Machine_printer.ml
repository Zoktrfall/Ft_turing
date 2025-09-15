let print_help () =
  print_endline "usage: ft_turing [-h] [--stats [--no-trace]] jsonfile input";
  print_endline "positional arguments:";
  print_endline "  jsonfile   json description of the machine";
  print_endline "  input      input of the machine";
  print_endline "optional arguments:";
  print_endline "  -h, --help   show this help message and exit";
  print_endline "  --stats      print time/space complexity stats after execution";
  print_endline "  --no-trace   only valid together with --stats; suppresses per-step trace"

let print_usage () =
  print_endline "usage: ft_turing [-h] jsonfile input"

let print_errors errs =
  List.iter (fun e -> prerr_endline (Machine_error.to_string e)) errs

let print_halt state =
  Printf.printf "HALT: final state %s\n%!" state

let print_blocked_no_state st =
  Printf.printf "BLOCKED: no transitions for state '%s'\n%!" st

let print_blocked_no_rule st c =
  Printf.printf "BLOCKED: no rule for (state=%s, read='%c')\n%!" st c

let print_step_limit n =
  Printf.printf "STOP: step limit (%d) reached\n%!" n

let render_tape ~blank ~left ~head ~right =
  let pad_right = 13 in
  let buf = Buffer.create 64 in
  Buffer.add_char buf '[';

  let rec add_left = function
    | [] -> ()
    | x :: xs -> add_left xs; Buffer.add_char buf x
  in
  add_left left;

  Buffer.add_char buf '<'; Buffer.add_char buf head; Buffer.add_char buf '>';

  List.iter (Buffer.add_char buf) right;

  for _i = 1 to pad_right do Buffer.add_char buf blank done;

  Buffer.add_char buf ']';
  Buffer.contents buf

let print_transition_line ~blank ~left ~head ~right ~state ~tr =
  let tape_s = render_tape ~blank ~left ~head ~right in
  Printf.printf "%s (%s, %c) -> (%s, %c, %s)\n%!"
    tape_s state head
    tr.Machine.to_state
    tr.Machine.write
    (Machine.string_of_action tr.Machine.action)


let star_line = String.make 80 '*'

let center_in_76 s =
  let w = 76 in
  let len = String.length s in
  if len >= w then String.sub s 0 w
  else
    let left = (w - len) / 2 in
    let right = w - len - left in
    String.make left ' ' ^ s ^ String.make right ' '

let print_box_title name =
  let empty_row = Printf.sprintf "* %-76s *" "" in
  let name_row  = Printf.sprintf "* %s *" (center_in_76 name) in
  Printf.printf "%s\n%s\n%s\n%s\n%!" star_line empty_row name_row empty_row

let read_priority = function
  | '.' -> 0
  | '1' -> 1
  | '-' -> 2
  | '=' -> 3
  | c   -> 10 + Char.code c

let compare_read (a,_) (b,_) =
  let pa = read_priority a and pb = read_priority b in
  if pa <> pb then Int.compare pa pb else Char.compare a b

let print_transitions (m : Machine.t) =
  List.iter (fun state ->
    match Machine.StringMap.find_opt state m.delta with
    | None -> ()
    | Some cmap ->
        let rules = Machine.CharMap.bindings cmap |> List.sort compare_read in
        List.iter (fun (read, tr) ->
          Printf.printf "(%s, %c) -> (%s, %c, %s)\n"
            state read tr.Machine.to_state tr.Machine.write
            (Machine.string_of_action tr.Machine.action)
        ) rules
  ) m.states

let print_machine_info (m : Machine.t) =
  print_box_title m.name;
  Printf.printf "%s\n" star_line;

  Printf.printf "Alphabet: [ %s ]\n"
    (String.concat ", " (List.map (String.make 1) m.alphabet));
  Printf.printf "States : [ %s ]\n" (String.concat ", " m.states);
  Printf.printf "Initial : %s\n" m.initial;
  Printf.printf "Finals : [ %s ]\n"
    (String.concat ", " m.finals);

  print_transitions m;

  Printf.printf "%s\n%!" star_line

(* Bonus *)
let print_stats ~input ~steps ~space ~min_pos ~max_pos =
  let n = String.length input in
  Printf.printf "----- stats -----\n";
  Printf.printf "input length (n) : %d\n" n;
  Printf.printf "steps            : %d\n" steps;
  Printf.printf "space (cells)    : %d\n" space;
  Printf.printf "min_pos          : %d\n" min_pos;
  Printf.printf "max_pos          : %d\n%!" max_pos