let all_ones s = String.length s > 0 && String.for_all (fun c -> c = '1') s
let err msg = Error [msg]

let validate_unary_sub s =
  if String.length s = 0 then err "empty input"
  else if s.[String.length s - 1] <> '=' then err "missing trailing '='"
  else
    let body = String.sub s 0 (String.length s - 1) in
    match String.split_on_char '-' body with
    | [lhs; rhs] ->
        if lhs = "" then err "left operand before '-' must be non-empty (at least one '1')"
        else if rhs = "" then err "right operand after '-' must be non-empty (at least one '1')"
        else if not (all_ones lhs) then err "left operand must contain only '1'"
        else if not (all_ones rhs) then err "right operand must contain only '1'"
        else if String.length rhs > String.length lhs then
          err "right operand must not be longer than left operand (would result in negative result)"
        else Ok ()
    | _ -> err "expected exactly one '-' separator"


let validate_unary_add s =
  if String.length s = 0 then err "empty input"
  else if s.[String.length s - 1] <> '=' then err "missing trailing '='"
  else
    let body = String.sub s 0 (String.length s - 1) in
    (match String.index_opt body '=' with
     | Some _ -> err "unexpected '=' inside input (only allowed as final trailing symbol)"
     | None ->
         match String.split_on_char '+' body with
         | [lhs; rhs] ->
             if lhs = "" then err "left operand before '+' must be non-empty (at least one '1')"
             else if rhs = "" then err "right operand after '+' must be non-empty (at least one '1')"
             else if not (all_ones lhs) then err "left operand must contain only '1'"
             else if not (all_ones rhs) then err "right operand must contain only '1'"
             else Ok ()
         | _ -> err "expected exactly one '+' separator")

let validate_palindrome s =
  if String.length s = 0 then err "empty input"
  else
    let invalid_char =
      String.to_seq s
      |> Seq.find (fun c -> c <> '0' && c <> '1')
    in
    match invalid_char with
    | Some c ->
        err (Printf.sprintf "invalid character '%c' in input (only '0' and '1' allowed)" c)
    | None -> Ok ()

let validate_for_machine ~machine_name ~input =
  match String.lowercase_ascii machine_name with
  | "unary_sub" -> validate_unary_sub input
  | "unary_add" -> validate_unary_add input
  | "palindrome_decider" -> validate_palindrome input
  | _ -> Ok ()