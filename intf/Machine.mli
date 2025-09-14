type action = Left | Right

val action_of_string : string -> (action, string) result
val string_of_action : action -> string

type transition = {
  read : char;
  write : char;
  to_state : string;
  action : action;
}

module CharMap : Map.S with type key = char
module StringMap : Map.S with type key = string

type t = {
  name : string;
  alphabet : char list;
  blank : char;
  states : string list;
  initial : string;
  finals : string list;
  delta : transition CharMap.t StringMap.t;
}

val validate_input : t -> string -> (unit, string list) result
