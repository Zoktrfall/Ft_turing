type action = Left | Right

let action_of_string = function
  | "LEFT" -> Ok Left
  | "RIGHT" -> Ok Right
  | s -> Error ("invalid action: " ^ s)

let string_of_action = function
  | Left -> "LEFT"
  | Right -> "RIGHT"

type transition = {
  read : char;
  write : char;
  to_state : string;
  action : action;
}

module CharMap = Map.Make (Char)
module StringMap = Map.Make (String)

type t = {
  name : string;
  alphabet : char list;
  blank : char;
  states : string list;
  initial : string;
  finals : string list;
  delta : transition CharMap.t StringMap.t;
}

let validate_input (m : t) (s : string) =
  let module CSet = Set.Make (Char) in
  let alpha =
    m.alphabet |> List.to_seq |> CSet.of_seq
  in
  
  let rec loop i acc =
    if i = String.length s then acc
    else
      let c = s.[i] in
      let acc =
        if not (CSet.mem c alpha) then
          (Printf.sprintf
             "input contains symbol not in alphabet: '%c' at index %d"
             c i) :: acc
        else acc
      in
      let acc =
        if Char.equal c m.blank then
          (Printf.sprintf
             "input must not contain the blank symbol '%c' (at index %d)"
             m.blank i) :: acc
        else acc
      in
      loop (i + 1) acc
  in

  let errs = loop 0 [] |> List.rev in
  match errs with
    | [] -> Ok ()
    | es -> Error es