open Machine

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


let normalize_edges ~(blank:char) (t : tape) : tape =
  let head = t.head in
  let left = t.left in
  let right = t.right in
  ignore blank;
  { left; head; right }

let move_left ~(blank:char) (t : tape) : tape =
  match t.left with
  | x :: xs -> { left = xs; head = x; right = t.head :: t.right }
  | []      -> { left = []; head = blank; right = t.head :: t.right }

let move_right ~(blank:char) (t : tape) : tape =
  match t.right with
  | x :: xs -> { left = t.head :: t.left; head = x; right = xs }
  | []      -> { left = t.head :: t.left; head = blank; right = [] }

let write (c:char) (t:tape) : tape = { t with head = c }

let squeeze_blanks ~(blank:char) (t:tape) : tape =
  let rec drop_left_blanks = function
    | [] -> []
    | x::xs ->
        let xs' = drop_left_blanks xs in
        if xs' = [] && Char.equal x blank then [] else x::xs'
  in
  let rec drop_right_blanks = function
    | [] -> []
    | x::xs ->
        if Char.equal x blank then drop_right_blanks xs else x::xs
  in
  { left = drop_left_blanks t.left; head = t.head; right = drop_right_blanks t.right }



let init ~(machine:Machine.t) ~(input:string) : config =
  let head, right =
    if String.length input = 0 then (machine.blank, [])
    else (input.[0], List.init (String.length input - 1) (fun i -> input.[i+1]))
  in
  { state = machine.initial; tape = { left = []; head; right }; steps = 0 }

  
let step ~(machine:Machine.t) (cfg : config) : (config, halt_reason) result =
  if List.exists (String.equal cfg.state) machine.finals
  then Error (Final_state cfg.state)
  else
    let open Machine in
    match StringMap.find_opt cfg.state machine.delta with
    | None -> Error (Blocked_no_state cfg.state)
    | Some smap ->
        let sym = cfg.tape.head in
        match CharMap.find_opt sym smap with
        | None -> Error (Blocked_no_rule (cfg.state, sym))
        | Some (tr : transition) ->
            let tape' = write tr.write cfg.tape in
            let tape'' =
              match tr.action with
              | Left  -> move_left ~blank:machine.blank tape'
              | Right -> move_right ~blank:machine.blank tape'
            in
            Ok { state = tr.to_state; tape = tape''; steps = cfg.steps + 1 }


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