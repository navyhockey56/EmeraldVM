open Instr

(*
	Converts the value into a human readable format
*)
let register_to_s = function 
	| `L_Int n -> "Reg( Int = " ^ (string_of_int n) ^ " )"
	| `L_Str str ->  "Reg ( String = " ^ str ^ " )"
	| `L_Id id -> "Reg ( ID = " ^ id ^ " )"
	| `L_Loc n -> "Reg( Pointer = " ^ (string_of_int n) ^ " )"
	| _ -> "Reg( Table )"
;;

(* 
	Extracts the value from a register
	@param [`L_Reg] The register to extract the value from
*)
let extract_reg_value = function 
	| `L_Reg n -> n
;;


(*
	Determines if the value is an int
	@param [value] The value to check
	@return [bool] True if the value is an `L_Int, false otherwise
*)
let is_int = function 
	| `L_Int _ -> true 
	| _ -> false 
;;

(*
	Determines if the value is an table point
	@param [value] The value to check
	@return [bool] True if the value is an `L_Loc, false otherwise
*)
let is_loc = function 
	| `L_Loc _ -> true 
	| _ -> false 
;;

let is_id = function 
	| `L_Id _ -> true 
	| _ -> false 

(*
	Get the register out of the registers map
	@param [regs] registers - The registers to retrieve from.
	@param [reg] register - The register to extract the value from
	@return [value] The value from the register
*)
let get_register_value registers register = 
	Hashtbl.find registers (extract_reg_value register)
;;


(*
	Upserts the given register within the registers table.
	@param [regs] registers - The registers to retrieve from.
	@param [reg] register - The register to extract the value from
	@return ()
*)
let update_register registers register new_value =
	Hashtbl.replace registers (extract_reg_value register) new_value
;;

(*
	Retrieves the top of the stack. Errors if the stack is empty
	@param [stack] The stack
	@return [string * int * regs] The top of the stack
*)
let get_top_of_stack stack =
	match stack with 
		| head::_ -> head
		| [] -> failwith "Fatal error: Stack is empty, cannot retrieve the first element."
;;

(*
	Extracts the contents of the top of the stack (function_name, program_countr, register)
	and returns them along with the tail of the stack. Errors if the stack is empty.
	@param [stack] The stack
	@return [string * int * regs * stack] The top of the stack and the tail of the stack.
*)
let extract_stack_contents stack =
	match stack with 
		| (function_name, program_counter, registers)::tail -> 
			(function_name, program_counter, registers, tail)
		| _ -> failwith "Fatal error: Stack is empty, cannot extract its contents"
;;

(*
	Retrieves the current instruction of the given program
	with the given config.

	@param [prog] p - The program
	@param [config] (heap, stack) - The program configuration

	@return [instr] The current instruction
*)
let current_instruction (p:prog) (stack):instr = 
	(* 
		Get the function name and program counter off the
	 	top of the stack, then retrieve the instruction at
	 	the location the program counter specifies.
	*)
	let (function_name, program_counter, _) = get_top_of_stack stack in 
	let rubevm_function = Hashtbl.find p function_name in 
	Array.get rubevm_function program_counter
;;


(*
	Converts an id to a value type.
	@param [id] The id to convert
	@return [value] A value object holding the id.
*)
let convert_id_to_value (x:id):value = 
	match x with  
		| `L_Id id -> (`L_Id id)
;;


(*
	Extracts from integer from a `L_Int value object.
*)
let extract_int_from_reg (r:value) = match r with 
	| `L_Int n -> n
	| _ -> failwith ("Fatal Error: Expected to find an Interger, but found a " ^ (register_to_s r))
;;


(*
	Extracts the id from a `L_Id object
*)	
let extract_id_from_reg = function 
	| `L_Id id -> id
	| `L_Str s -> failwith (String.concat "L_Str is not a L_Id" [s])
	| `L_Int _ -> failwith "L_Int is not a L_Id"
	| `L_Loc _ -> failwith "L_Loc is not a L_Id"
	| `L_Tab _ -> failwith "L_Tab is not a L_Id"
	| _ -> failwith "non-id value"
;;


(*
	Extracts the value from a table value
	@param [`L_Tab] tab - The table value
	@return 
*)	
let extract_table_from_reg tab = match tab with 
	| `L_Tab tbl -> tbl 
	| _ -> failwith ("Fatal Error: Expected to a Table, but found a " ^ (register_to_s tab))
;;

(*
	Extracts the string from a string value.
	@param [`L_Str] s - The string value
	@return [string] The contained string
*)	
let extract_string_from_reg s = match s with 
	| `L_Str str -> str 
	| _ -> failwith ("Fatal Error: Expected to a String, but found a " ^ (register_to_s s))
;;

(*
	Determines if the two given values are equal.

	@param [value] r1 - The first value
	@param [value] r2 - The second value
 *)
let are_registers_equal r1 r2 = match (r1,r2) with
	| (`L_Int n1, `L_Int n2) -> n1 = n2
	| (`L_Str str1, `L_Str str2)->  str1 = str2
	| (`L_Id id1, `L_Id id2) -> id1 = id2
	| (`L_Loc n1, `L_Loc n2) -> n1 = n2
	| _ -> false
;;

(*
	Provides the next available location for
	a register.

	We are operatoring on the assumption that we have
	infinite registers, so the next location is just
	the next consecutive integer from the previous
	call.

	While this looks like a method call, it's actually
	just a variable. We are defining a lambda, and embedding
	within that lambda a referenced integer which will
	be incremented and returned with each execution of this
	lamda.
*)
let next_location =
	let x = ref 0 in 
	(fun () -> x := !x + 1; !x)
;;

(*
	Retrieves the start register location from a function call
*)
let get_start_location inst = match inst with  
	| I_call (_,n1,_) -> n1
	| I_const _ -> failwith "invalid program location - get_n1 - const"
	| I_mul _ -> failwith "invalid program location - get_n1 - mul"
	| I_sub _ -> failwith "invalid program location - get_n1 - sub"
	| I_leq _ -> failwith "invalid program location - get_n1 - leq"
	| I_if_zero _ -> failwith "invalid program location - get_n1 - if 0"
	| I_mov _ -> failwith "invalid program location - get_n1 - mov"
	| _ -> failwith "invalid program location - get_n1"
;;


(*
	Copies the values from start_register to end_register within registers into
	new registers. 
	The copied values in new_registers will be keyed consequetively starting at 0.
*)
let rec copy_registers current_position start_register end_register registers new_registers = 
	(* Base case: Return the copied registers *)
	if current_position > (end_register - start_register) then new_registers
	(* Copy the value at current_position out of registers and into the new_registers *)
	else begin
		(* Get the value *)
		let value = Hashtbl.find registers (start_register + current_position) in
		(* Store it in new registers *)
		Hashtbl.replace new_registers current_position value; 
		(* Continue the copy *)
		copy_registers (current_position + 1) start_register end_register registers new_registers
	end
;;




