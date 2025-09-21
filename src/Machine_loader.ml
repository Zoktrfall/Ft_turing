open Machine_error

(* This is a tiny “result monad” helper.
  It means: if r = Ok x, continue with f x; if r = Error e, stop right there and propagate the error.
  It makes a long sequence of validations read top-to-bottom without nested matches. *)
let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

module J = struct
  open Yojson.Safe

  let from_file path =
    try Ok (Yojson.Safe.from_file path) with
    | Sys_error e -> Error [Json ("cannot read file: " ^ e)]
    | Yojson.Json_error e -> Error [Json ("parse error: " ^ e)]

  let as_obj = function
    | `Assoc kvs -> Ok kvs
    | _ -> Error [Json "root must be an object"]

  let expect_obj field = function
    | `Assoc kvs -> Ok kvs
    | _ -> Error [FieldType (field, "object")]

  let find name obj =
    match List.assoc_opt name obj with
    | Some v -> Ok v
    | None -> Error [FieldMissing name]

  let as_string field = function
    | `String s -> Ok s
    | _ -> Error [FieldType (field, "string")]

  let as_list field = function
    | `List xs -> Ok xs
    | _ -> Error [FieldType (field, "list")]
end


let ensure_unique ~eq ~on_dup lst =
  let rec go seen = function
    | [] -> Ok ()
    | x :: xs ->
        if List.exists (eq x) seen then Error [on_dup x]
        else go (x :: seen) xs
  in
  go [] lst

let char_of_len1 s =
  if String.length s = 1 then Ok s.[0] else Error [AlphabetSymbolLen s]



let load ~json_path =
  let open Yojson.Safe in

  let* json = J.from_file json_path in
  let* root = J.as_obj json in
  let* name_v = J.find "name" root in
  let* alphabet_v = J.find "alphabet" root in
  let* blank_v = J.find "blank" root in
  let* states_v = J.find "states" root in
  let* initial_v = J.find "initial" root in
  let* finals_v = J.find "finals" root in
  let* transitions_v = J.find "transitions" root in

  (* For names, I actually added some validation, but I’m not sure whether I should allow other names.
   For now, any names are allowed, but we have predefined ones for stricter validation:
    unary_sub, 
    unary_add, 
    palindrome_decider, 
    zero_2n, 
    zero_n_one_n, 
    meta_run_unary_add. *)
  let* name = J.as_string "name" name_v in

  let* alphabet_list = J.as_list "alphabet" alphabet_v in
  let rec map_chars acc = function
    | [] -> Ok (List.rev acc)
    | (`String s) :: tl ->
        let* c = char_of_len1 s in
        map_chars (c :: acc) tl
    | _ :: _ -> Error [FieldType ("alphabet[]", "string")]
  in
  let* alphabet = map_chars [] alphabet_list in
  let* () = ensure_unique ~eq:Char.equal ~on_dup:(fun c -> AlphabetDuplicate c) alphabet in

  let* blank_s = J.as_string "blank" blank_v in
  let* blank =
    match char_of_len1 blank_s with
    | Ok c -> Ok c
    | Error e -> Error e
  in

  let* () =
    if List.exists (Char.equal blank) alphabet
    then Ok ()
    else Error [BlankNotInAlphabet blank]
  in

  let* states_list = J.as_list "states" states_v in
  let rec map_states acc = function
    | [] -> Ok (List.rev acc)
    | (`String s) :: tl -> map_states (s :: acc) tl
    | _ :: _ -> Error [FieldType ("states[]", "string")]
  in
  let* states = map_states [] states_list in
  let* () = ensure_unique ~eq:String.equal ~on_dup:(fun s -> StatesDuplicate s) states in

  let* initial = J.as_string "initial" initial_v in
  let* () =
    if List.exists (String.equal initial) states then Ok ()
    else Error [InitialNotInStates initial]
  in

  let* finals_list = J.as_list "finals" finals_v in
  let rec map_finals acc = function
    | [] -> Ok (List.rev acc)
    | (`String s) :: tl -> map_finals (s :: acc) tl
    | _ :: _ -> Error [FieldType ("finals[]", "string")]
  in
  let* finals = map_finals [] finals_list in
  let unknown_finals =
    List.filter (fun f -> not (List.exists (String.equal f) states)) finals
  in
  let* () =
    if unknown_finals = [] then Ok ()
    else Error [FinalsNotSubset unknown_finals]
  in

  let* transitions_obj = J.expect_obj "transitions" transitions_v in

  let module CSet = Set.Make (Char) in
  let alpha_set = alphabet |> List.to_seq |> CSet.of_seq in

  let module CM = Machine.CharMap in
  let module SM = Machine.StringMap in

  let add_state ((acc : (Machine.transition CM.t) SM.t), errs) (st, v) =
    match errs with
    | _ :: _ -> (acc, errs)
    | [] ->
        if not (List.exists (String.equal st) states)
        then (acc, [TransitionStateUnknown st])
        else
          match v with
          | `List trs ->
              let add_one (cmap : Machine.transition CM.t) (idx : int) (js : Yojson.Safe.t)
                : (Machine.transition CM.t, Machine_error.t list) result =
                match js with
                | `Assoc kvs ->
                    let get k =
                      match List.assoc_opt k kvs with
                      | Some x -> Ok x
                      | None ->
                          Error [TransitionMalformed ("state " ^ st ^ " (missing " ^ k ^ ")")]
                    in
                    let* read_js    = get "read" in
                    let* to_state_js= get "to_state" in
                    let* write_js   = get "write" in
                    let* action_js  = get "action" in

                    let* read_s  = J.as_string "read" read_js in
                    let* write_s = J.as_string "write" write_js in

                    let* read_c =
                      match char_of_len1 read_s with
                      | Ok c when CSet.mem c alpha_set -> Ok c
                      | Ok c -> Error [TransitionSymbolNotInAlphabet (c, "state " ^ st)]
                      | Error e -> Error e
                    in
                    let* write_c =
                      match char_of_len1 write_s with
                      | Ok c when CSet.mem c alpha_set -> Ok c
                      | Ok c -> Error [TransitionSymbolNotInAlphabet (c, "state " ^ st)]
                      | Error e -> Error e
                    in
                    let* to_state =
                      match to_state_js with
                      | `String s ->
                          if List.exists (String.equal s) states then Ok s
                          else Error [TransitionToStateUnknown s]
                      | _ -> Error [FieldType ("to_state", "string")]
                    in
                    let* action =
                      match action_js with
                      | `String s ->
                          (match Machine.action_of_string s with
                          | Ok a -> Ok a
                          | Error _ -> Error [TransitionActionInvalid s])
                      | _ -> Error [FieldType ("action", "string")]
                    in

                    let tr = Machine.{ read = read_c; write = write_c; to_state; action } in
                    if CM.mem read_c cmap
                    then Error [TransitionDuplicateRead (st, read_c)]
                    else Ok (CM.add read_c tr cmap)

                | _ ->
                    Error [TransitionMalformed ("state " ^ st ^ " (entry " ^ string_of_int idx ^ ")")]
              in

              let rec fold_i i cmap errs = function
                | [] -> (cmap, errs)
                | x :: xs ->
                    begin match add_one cmap i x with
                    | Ok cmap' -> fold_i (i + 1) cmap' errs xs
                    | Error es' -> fold_i (i + 1) cmap (errs @ es') xs
                    end
              in

              let cmap, errs' = fold_i 0 CM.empty [] trs in
              if errs' = [] then (SM.add st cmap acc, []) else (acc, errs')
          | _ -> (acc, [FieldType ("transitions[" ^ st ^ "]", "list")])
  in

  let delta, terrs =
    List.fold_left add_state (SM.empty, []) transitions_obj
  in
  if terrs <> [] then Error terrs
  else
    Ok Machine.
      { name; alphabet; blank; states; initial; finals; delta }


let validate_input ~machine ~input =
  match Machine.validate_input machine input with
  | Ok () -> Ok ()
  | Error msgs ->
      let errs = List.map (fun s -> Json s) msgs in
      Error errs