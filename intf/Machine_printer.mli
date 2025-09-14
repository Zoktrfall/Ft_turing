val print_help : unit -> unit
val print_usage : unit -> unit

val print_errors : Machine_error.t list -> unit

val print_halt : string -> unit

val print_machine_info : Machine.t -> unit

val print_blocked_no_state : string -> unit
val print_blocked_no_rule : string -> char -> unit
val print_step_limit : int -> unit

val render_tape :
  blank:char ->
  left:char list ->
  head:char ->
  right:char list ->
  string

val print_transition_line :
  blank:char ->
  left:char list ->
  head:char ->
  right:char list ->
  state:string ->
  tr:Machine.transition ->
  unit

(* Only used for bonus part *)
val print_stats :
  input:string ->
  steps:int ->
  space:int ->
  min_pos:int ->
  max_pos:int ->
  unit