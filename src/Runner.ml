type tape = {
  left : char list;
  head : char;
  right : char list;
}

type config = {
  state : string;
  tape : tape;
  steps : int;
}

type halt_reason =
  | Final_state of string
  | Blocked_no_state of string
  | Blocked_no_rule of string * char
  | Step_limit_reached of int

let move_left ~(blank:char) (t : tape) : tape =
  match t.left with
  | x :: xs -> { left = xs; head = x; right = t.head :: t.right }
  | [] -> { left = []; head = blank; right = t.head :: t.right }

let move_right ~(blank:char) (t : tape) : tape =
  match t.right with
  | x :: xs -> { left = t.head :: t.left; head = x; right = xs }
  | [] -> { left = t.head :: t.left; head = blank; right = [] }

let write (c:char) (t:tape) : tape = { t with head = c }

let init ~(machine:Machine.t) ~(input:string) : config =
  let head, right =
    if String.length input = 0 then (machine.blank, [])
    else (input.[0], List.init (String.length input - 1) (fun i -> input.[i+1]))
  in
  { state = machine.initial; tape = { left = []; head; right }; steps = 0 }

let run ~(machine:Machine.t) ~(input:string) ?(max_steps=100) ()
  : (config, halt_reason) result =
  let rec loop cfg =
    if cfg.steps >= max_steps then Error (Step_limit_reached max_steps)
    else
      match Machine.StringMap.find_opt cfg.state machine.delta with
      | None -> Error (Blocked_no_state cfg.state)
      | Some cmap ->
          let sym = cfg.tape.head in
          match Machine.CharMap.find_opt sym cmap with
          | None -> Error (Blocked_no_rule (cfg.state, sym))
          | Some tr ->
            (* Originally this was a single call:
              Machine_printer.print_transition_line ~machine cfg tr
              But to avoid cyclic dependencies (MachinePrinter depending on Runner types),
              we now pass tape fields and state explicitly.
              Functionally it is equivalent — just more explicit. *)
            Machine_printer.print_transition_line
              ~blank:machine.blank
              ~left:cfg.tape.left
              ~head:cfg.tape.head
              ~right:cfg.tape.right
              ~state:cfg.state
              ~tr;

              if List.exists (String.equal tr.to_state) machine.finals then
                Error (Final_state tr.to_state)
              else
                let tape' = write tr.write cfg.tape in
                let tape'' =
                  match tr.action with
                  | Left  -> move_left ~blank:machine.blank tape'
                  | Right -> move_right ~blank:machine.blank tape'
                in
                let cfg' = { state = tr.to_state; tape = tape''; steps = cfg.steps + 1 } in
                loop cfg'
  in
  let cfg0 = init ~machine ~input in
  loop cfg0


(* Bonus part *)
type stats = {
  n : int; 
  steps : int;
  min_pos : int;
  max_pos : int;
  space : int;  
  moves_left : int;
  moves_right : int;
  writes_changed : int;
  state_visits : (string * int) list;
  final_state : string option;       
  theory_time : string option;       
  theory_space : string option; 
}

(* We keep print_stats here (in Runner) instead of Machine_printer to avoid
   introducing a circular dependency between Runner and Machine_printer.
   This keeps the module graph simple and avoids extra refactoring work. *)
(* I Fucked up *)
let print_stats (st : stats) =
  let open Printf in
  printf "----- stats -----\n";
  printf "input length (n) : %d\n" (st.n);
  printf "steps            : %d\n" st.steps;
  printf "space (cells)    : %d\n" st.space;
  printf "min_pos          : %d\n" st.min_pos;
  printf "max_pos          : %d\n" st.max_pos;
  printf "moves L / R      : %d / %d\n" st.moves_left st.moves_right;
  printf "writes (changed) : %d\n" st.writes_changed;
  (match st.final_state with
   | Some s -> printf "final state      : %s\n" s
   | None -> ());
  printf "state visits     : {";
  let first = ref true in
  List.iter (fun (k,v) ->
    if not !first then printf ", ";
    first := false;
    printf "%s:%d" k v
  ) st.state_visits;
  printf "}\n";
  (match st.theory_time with Some s -> printf "theory time      : %s\n" s | None -> ());
  (match st.theory_space with Some s -> printf "theory space     : %s\n" s | None -> ());
  flush stdout

let stats_make ~n ~steps ~min_pos ~max_pos ~moves_left ~moves_right
               ~writes_changed ~state_visits ~final_state
               ~theory_time ~theory_space =
  let space = (max_pos - min_pos + 1) in
  { n; steps; min_pos; max_pos; space;
    moves_left; moves_right; writes_changed;
    state_visits; final_state; theory_time; theory_space }

let incr_count (m : int Machine.StringMap.t) (k : string) =
  let old = match Machine.StringMap.find_opt k m with Some v -> v | None -> 0 in
  Machine.StringMap.add k (old + 1) m

let theory_for (machine_name : string) ~(input:string) : string option * string option =
  match String.lowercase_ascii machine_name with
  | "unary_sub" ->
      let l, r =
        match String.split_on_char '=' input with
        | body :: _ ->
            (match String.split_on_char '-' body with
             | [lhs; rhs] ->
                 let count_ones s =
                   let c = ref 0 in
                   String.iter (fun ch -> if ch = '1' then incr c) s;
                   !c
                 in
                 (count_ones lhs, count_ones rhs)
             | _ -> (0, 0))
        | _ -> (0, 0)
      in
      let t_note =
        Printf.sprintf
          "time Θ((A+B)·B); here A=%d, B=%d → Θ(%d·%d) ≈ Θ(%d). worst-case Θ(n²), best-case Θ(n)"
          l r (l + r) r ((l + r) * r)
      in
      let s_note = "space Θ(n) (linear in input length)" in
      (Some t_note, Some s_note)

  | "unary_add" ->
    let l, r =
      match String.split_on_char '=' input with
      | body :: _ -> (
          match String.split_on_char '+' body with
          | [lhs; rhs] ->
              let count_ones s =
                let c = ref 0 in
                String.iter (fun ch -> if ch = '1' then incr c) s;
                !c
              in
              (count_ones lhs, count_ones rhs)
          | _ -> (0, 0))
      | _ -> (0, 0)
    in
    let t_note =
      Printf.sprintf
        "time Θ((A+B)·B); here A=%d, B=%d → Θ(%d·%d) ≈ Θ(%d). worst-case Θ(n²), best-case Θ(n)"
        l r (l + r) r ((l + r) * r)
    in
    let s_note = "space Θ(n) (linear in input length)" in
    (Some t_note, Some s_note)
  
  | "palindrome_decider" ->
      let n =
        match String.index_opt input '.' with
        | Some i -> i
        | None -> String.length input
      in
      let t_note =
        "time Θ(n²) worst-case, Θ(n) best-case (scans tape each round until mismatch or finish)"
      in
      let s_note =
        Printf.sprintf
          "space Θ(n) (in-place marking + 1 output symbol; input length ≈ %d)" n
      in
      (Some t_note, Some s_note)

  | "zero_n_one_n" ->
    let t_note = "time Θ(n²) worst-case, Θ(n) best-case (pair-and-rewind rounds)" in
    let s_note = "space Θ(n) (in-place marks + one output symbol)" in
    (Some t_note, Some s_note)

  | "zero_2n" ->
    let len = String.length input in
    let t_note = "time Θ(n) (single left-to-right scan with parity tracking)" in
    let s_note = Printf.sprintf "space Θ(n) (input length = %d, no additional tape cells used)" len in
    (Some t_note, Some s_note)

  | "meta_run_unary_add" ->
    (Some "time ≈ time(unary_add) (decoder O(1) + addition)",
     Some "space ≈ space(unary_add)")
  
  | _ -> (None, None)


let run_with_stats
    ?(max_steps=100)
    ?(trace=true)
    ~(machine:Machine.t)
    ~(input:string)
    ()
  : (halt_reason * stats) =

  let n = String.length input in
  let (theory_time, theory_space) = theory_for machine.name ~input in

  let rec loop (cfg:config)
               (head_pos:int)
               (min_pos:int)
               (max_pos:int)
               (moves_l:int)
               (moves_r:int)
               (writes_changed:int)
               (visits:int Machine.StringMap.t)
    : (halt_reason * stats) =

    if cfg.steps >= max_steps then
      let st = stats_make ~n ~steps:cfg.steps ~min_pos ~max_pos
                 ~moves_left:moves_l ~moves_right:moves_r ~writes_changed
                 ~state_visits:(Machine.StringMap.bindings visits)
                 ~final_state:None
                 ~theory_time ~theory_space
      in
      (Step_limit_reached max_steps, st)
    else
      match Machine.StringMap.find_opt cfg.state machine.delta with
      | None ->
          let st = stats_make ~n ~steps:cfg.steps ~min_pos ~max_pos
                     ~moves_left:moves_l ~moves_right:moves_r ~writes_changed
                     ~state_visits:(Machine.StringMap.bindings visits)
                     ~final_state:None
                     ~theory_time ~theory_space
          in
          (Blocked_no_state cfg.state, st)
      | Some cmap ->
          let sym = cfg.tape.head in
          match Machine.CharMap.find_opt sym cmap with
          | None ->
              let st = stats_make ~n ~steps:cfg.steps ~min_pos ~max_pos
                         ~moves_left:moves_l ~moves_right:moves_r ~writes_changed
                         ~state_visits:(Machine.StringMap.bindings visits)
                         ~final_state:None
                         ~theory_time ~theory_space
              in
              (Blocked_no_rule (cfg.state, sym), st)
          | Some tr ->
              if trace then
                Machine_printer.print_transition_line
                  ~blank:machine.blank
                  ~left:cfg.tape.left ~head:cfg.tape.head ~right:cfg.tape.right
                  ~state:cfg.state ~tr;

              let steps' = cfg.steps + 1 in
              let visits' = incr_count visits cfg.state in

              if List.exists (String.equal tr.to_state) machine.finals then
                let st = stats_make ~n ~steps:steps' ~min_pos ~max_pos
                           ~moves_left:moves_l ~moves_right:moves_r ~writes_changed
                           ~state_visits:(Machine.StringMap.bindings visits')
                           ~final_state:(Some tr.to_state)
                           ~theory_time ~theory_space
                in
                (Final_state tr.to_state, st)
              else
                let wrote_change = if tr.write <> cfg.tape.head then 1 else 0 in
                let tape' = write tr.write cfg.tape in
                let tape'', head_pos', moves_l', moves_r' =
                  match tr.action with
                  | Left -> (move_left ~blank:machine.blank tape', head_pos - 1, moves_l + 1, moves_r)
                  | Right -> (move_right ~blank:machine.blank tape', head_pos + 1, moves_l, moves_r + 1)
                in
                let min_pos' = if head_pos' < min_pos then head_pos' else min_pos in
                let max_pos' = if head_pos' > max_pos then head_pos' else max_pos in
                let cfg' = { state = tr.to_state; tape = tape''; steps = steps' } in
                loop cfg' head_pos' min_pos' max_pos' moves_l' moves_r'
                     (writes_changed + wrote_change) visits'
  in
  let cfg0 = init ~machine ~input in
  let result, st = loop cfg0 0 0 0 0 0 0 Machine.StringMap.empty in
  (result, st)