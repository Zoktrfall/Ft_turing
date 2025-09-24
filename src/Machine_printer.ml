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
  Printf.printf "STOP: step limit (%d) reached — machine likely stuck due to invalid JSON description.\n%!" n

let _fixed_inner_width : int option ref = ref None

let rec first_n n xs =
  match n, xs with
  | 0, _ | _, [] -> []
  | n, x :: tl -> x :: first_n (n - 1) tl

let drop n xs =
  let rec go k ys =
    match k, ys with
    | 0, _ | _, [] -> ys
    | k, _ :: tl -> go (k - 1) tl
  in
  go n xs

let last_n n xs =
  let len = List.length xs in
  if len <= n then xs else drop (len - n) xs

let render_tape ~blank ~left ~head ~right =
  let pad_right = 13 in

  let curr_inner_len =
    List.length left + 3 + List.length right + pad_right
  in
  let target_inner_len =
    match !_fixed_inner_width with
    | Some w -> w
    | None ->
        _fixed_inner_width := Some curr_inner_len;
        curr_inner_len
  in

  let space_for_lr = max 0 (target_inner_len - pad_right - 3) in
  let take_left = min (List.length left) space_for_lr in
  let left_slice = last_n take_left left in
  let space_left_for_right = space_for_lr - List.length left_slice in
  let right_slice = first_n (max 0 space_left_for_right) right in

  let buf = Buffer.create (target_inner_len + 2) in
  Buffer.add_char buf '[';

  let rec add_left_rev_to_buf = function
    | [] -> ()
    | x :: xs -> add_left_rev_to_buf xs; Buffer.add_char buf x
  in
  add_left_rev_to_buf left_slice;

  Buffer.add_char buf '<';
  Buffer.add_char buf head;
  Buffer.add_char buf '>';

  List.iter (Buffer.add_char buf) right_slice;

  let already = List.length left_slice + 3 + List.length right_slice in
  let need_blanks = max 0 (target_inner_len - already) in
  for _ = 1 to need_blanks do Buffer.add_char buf blank done;

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