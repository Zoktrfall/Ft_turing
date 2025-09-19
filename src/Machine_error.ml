type t =
  | Json of string
  | FieldMissing of string
  | FieldType of string * string
  | AlphabetSymbolLen of string
  | AlphabetDuplicate of char
  | BlankNotInAlphabet of char
  | StatesDuplicate of string
  | InitialNotInStates of string
  | FinalsNotSubset of string list
  | TransitionStateUnknown of string
  | TransitionMalformed of string
  | TransitionSymbolNotInAlphabet of char * string
  | TransitionToStateUnknown of string
  | TransitionActionInvalid of string
  | TransitionDuplicateRead of string * char

let to_string = function
  | Json msg -> "validation error: " ^ msg
  | FieldMissing f -> "missing field: " ^ f
  | FieldType (f, exp) -> "field type error: " ^ f ^ " (expected " ^ exp ^ ")"
  | AlphabetSymbolLen s -> "alphabet entries must be length 1, got: " ^ s
  | AlphabetDuplicate c -> "duplicate symbol in alphabet: " ^ String.make 1 c
  | BlankNotInAlphabet c ->
      "blank symbol not in alphabet: " ^ String.make 1 c
  | StatesDuplicate s -> "duplicate state: " ^ s
  | InitialNotInStates s -> "initial not in states: " ^ s
  | FinalsNotSubset xs ->
      "finals contain unknown states: " ^ String.concat ", " xs
  | TransitionStateUnknown s -> "transitions for unknown state: " ^ s
  | TransitionMalformed ctx -> "malformed transition in " ^ ctx
  | TransitionSymbolNotInAlphabet (c, ctx) ->
      "transition uses symbol not in alphabet: "
      ^ String.make 1 c ^ " in " ^ ctx
  | TransitionToStateUnknown s ->
      "transition to unknown state: " ^ s
  | TransitionActionInvalid s -> "invalid action: " ^ s
  | TransitionDuplicateRead (st, c) ->
      "duplicate transition for state " ^ st ^ " on read "
      ^ String.make 1 c
