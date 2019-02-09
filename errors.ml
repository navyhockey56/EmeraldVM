open Instr

(*
	Converts the value into a human readable format
*)
let register_to_s = function 
	| `L_Int n -> "Reg( Int = " ^ (string_of_int n) ^ " )"
	| `L_Str str ->  "Reg( String = " ^ str ^ " )"
	| `L_Id id -> "Reg( ID = " ^ id ^ " )"
	| `L_Loc n -> "Reg( Pointer = " ^ (string_of_int n) ^ " )"
	| _ -> "Reg( Table )"
;;

let throw_general_error error_name message = 
	failwith (error_name ^ " - " ^ message)
;;

let throw_instruction_error error_name instrunction_name message =
	throw_general_error (error_name ^ " (" ^ instrunction_name ^ ")")  message
;;

let throw_illegal_argument instrunction_name expected_type position value =
	throw_instruction_error "Illegal Argument" instrunction_name (
		"Expected to find a " ^ expected_type ^ " in the " ^ position ^ 
		" argument, but found " ^ (register_to_s value)
	)
;;

let throw_table_not_found instrunction_name pointer =
	throw_instruction_error "Table not Found" instrunction_name ( 
		"The pointer " ^ (register_to_s pointer) ^ " is not bound within the heap."
	)
;;

let throw_function_not_found function_name =
	throw_instruction_error "Function not Found" "call" (
		"The function \"" ^ function_name ^ "\" is not defined."
	)
;;

let throw_global_not_defined variable_name = 
	throw_instruction_error "Global Variable not Found" "rd_glob" (
		"The variable \"" ^ variable_name ^ "\" is not bound wthin the heap."
	)
;;