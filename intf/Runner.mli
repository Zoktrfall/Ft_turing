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

val init : machine:Machine.t -> input:string -> config

val run : ?max_steps:int -> machine:Machine.t -> input:string -> (config, halt_reason) result


(* We Seperate the Bonus part *)
type stats = {
  steps     : int;
  min_pos   : int;
  max_pos   : int;
  space     : int;
}

val run_with_stats :
  ?max_steps:int ->
  ?trace:bool ->
  machine:Machine.t ->
  input:string ->
  unit ->
  (halt_reason * stats)