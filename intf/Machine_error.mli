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

val to_string : t -> string