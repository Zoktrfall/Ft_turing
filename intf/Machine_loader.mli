val load : json_path:string -> (Machine.t, Machine_error.t list) result
val validate_input : machine:Machine.t -> input:string -> (unit, Machine_error.t list) result