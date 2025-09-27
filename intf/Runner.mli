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

val init : machine:Machine.t -> input:string -> config

val run : ?max_steps:int -> machine:Machine.t -> input:string -> (config, halt_reason) result


(* We Seperate the Bonus part *)
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

(* You can check comments in ml file *)
val print_stats : stats -> unit

val run_with_stats :
  ?max_steps:int ->
  ?trace:bool ->
  machine:Machine.t ->
  input:string ->
  unit ->
  (halt_reason * stats)