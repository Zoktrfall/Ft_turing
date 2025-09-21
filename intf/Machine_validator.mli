val validate_unary_sub : string -> (unit, string list) result
val validate_unary_add : string -> (unit, string list) result
val validate_binary_input : string -> (unit, string list) result
val validate_zero_2n : string -> (unit, string list) result
val validate_meta_unary_add : string -> (unit, string list) result

val validate_for_machine : machine_name:string -> input:string -> (unit, string list) result