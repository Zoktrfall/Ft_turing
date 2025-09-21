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

let validate_binary_input s =
  if String.length s = 0 then err "empty input"
  else
    let bad =
      String.to_seq s |> Seq.find (fun c -> c <> '0' && c <> '1')
    in
    match bad with
    | Some c -> Error [Printf.sprintf "invalid character '%c' (only '0' and '1' allowed)" c]
    | None -> Ok ()

let validate_zero_2n s =
  if String.length s = 0 then
    err "input must not be empty"
  else
    match String.to_seq s |> Seq.find (fun c -> c <> '0') with
    | Some c -> err (Printf.sprintf "invalid character '%c' (only '0' allowed)" c)
    | None -> Ok ()


let validate_meta_unary_add s =
  if String.length s < 2 || s.[0] <> 'A' || s.[1] <> ':' then
    Error ["input must start with \"A:\""]
  else
    let payload = String.sub s 2 (String.length s - 2) in
    validate_unary_add payload

let validate_for_machine ~machine_name ~input =
  match String.lowercase_ascii machine_name with
  | "unary_sub" -> validate_unary_sub input
  | "unary_add" -> validate_unary_add input
  | "palindrome_decider" -> validate_binary_input input
  | "zero_n_one_n" -> validate_binary_input input
  | "zero_2n" -> validate_zero_2n input
  | "meta_run_unary_add" -> validate_meta_unary_add input
  | _ -> Ok ()