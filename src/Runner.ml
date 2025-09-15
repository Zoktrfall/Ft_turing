type tape = {
  left  : char list;
  head  : char;
  right : char list;
}

type config = {
  state : string;
  tape  : tape;
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
  | []      -> { left = []; head = blank; right = t.head :: t.right }

let move_right ~(blank:char) (t : tape) : tape =
  match t.right with
  | x :: xs -> { left = t.head :: t.left; head = x; right = xs }
  | []      -> { left = t.head :: t.left; head = blank; right = [] }

let write (c:char) (t:tape) : tape = { t with head = c }

let init ~(machine:Machine.t) ~(input:string) : config =
  let head, right =
    if String.length input = 0 then (machine.blank, [])
    else (input.[0], List.init (String.length input - 1) (fun i -> input.[i+1]))
  in
  { state = machine.initial; tape = { left = []; head; right }; steps = 0 }

let run ~(machine:Machine.t) ~(input:string) ?(max_steps=1_000_000) ()
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
  steps   : int;
  min_pos : int;
  max_pos : int;
  space   : int;
}

let run_with_stats
    ?(max_steps=1_000_000)
    ?(trace=true)
    ~(machine:Machine.t)
    ~(input:string)
    ()
  : (halt_reason * stats) =
  let rec loop (cfg:config) (head_pos:int) (min_pos:int) (max_pos:int) =
    if cfg.steps >= max_steps then
      (Step_limit_reached max_steps,
       { steps = cfg.steps; min_pos; max_pos; space = (max_pos - min_pos + 1) })
    else
      match Machine.StringMap.find_opt cfg.state machine.delta with
      | None ->
          (Blocked_no_state cfg.state,
           { steps = cfg.steps; min_pos; max_pos; space = (max_pos - min_pos + 1) })
      | Some cmap ->
          let sym = cfg.tape.head in
          match Machine.CharMap.find_opt sym cmap with
          | None ->
              (Blocked_no_rule (cfg.state, sym),
               { steps = cfg.steps; min_pos; max_pos; space = (max_pos - min_pos + 1) })
          | Some tr ->
              if trace then
                Machine_printer.print_transition_line
                  ~blank:machine.blank
                  ~left:cfg.tape.left ~head:cfg.tape.head ~right:cfg.tape.right
                  ~state:cfg.state ~tr;
              let steps' = cfg.steps + 1 in
              if List.exists (String.equal tr.to_state) machine.finals then
                (Final_state tr.to_state,
                 { steps = steps'; min_pos; max_pos; space = (max_pos - min_pos + 1) })
              else
                let tape' = write tr.write cfg.tape in
                let tape'', head_pos' =
                  match tr.action with
                  | Left  -> (move_left  ~blank:machine.blank tape', head_pos - 1)
                  | Right -> (move_right ~blank:machine.blank tape', head_pos + 1)
                in
                let min_pos' = if head_pos' < min_pos then head_pos' else min_pos in
                let max_pos' = if head_pos' > max_pos then head_pos' else max_pos in
                let cfg' = { state = tr.to_state; tape = tape''; steps = steps' } in
                loop cfg' head_pos' min_pos' max_pos'
  in
  let cfg0 = init ~machine ~input in
  loop cfg0 0 0 0