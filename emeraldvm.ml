open Instr
open Helpers

type prog_ret = [ `Reg of value | `Halt of value ]

(*
	Handles binary operator instructions (add, sub, mul, div, lt, leq).
	Note: True is represented by 1, false by 0.

	@param (heap, stack) - The heap and stack
	@param (reg, reg, reg) - The registers in the instruction
	@param (int -> int -> int) - A lambda of the binary operation 
	@ret (heap, stack) The updated heap and stack
*)	
let run_binary_operator (heap, stack) (r1, r2, r3) binary_op instr_name =
	let (function_name, program_counter, registers, tail) = Helpers.extract_stack_contents stack in 
	
	(* Extract the values *)
	let v1 = Helpers.get_register_value registers r2 in 
	let v2 = Helpers.get_register_value registers r3 in 

	(* Check the the r1 is an int *)
	if not (Helpers.is_int v1) then
		failwith (
			"Illegal call to " ^ instr_name ^ ". Excepted the 2nd register to contain an integer, " ^ 
			"but found: " ^ (Helpers.register_to_s v1)
		)
	;
	(* Check that r2 is an int *)
	if not (Helpers.is_int v2) then
		failwith (
			"Illegal call to " ^ instr_name ^ ". Excepted 3rd register to contain an integer, " ^ 
			"but found: " ^ (Helpers.register_to_s v2)
		)
	;

	(* Extract the integers *)
	let v1 = Helpers.extract_int_from_reg v1 in
	let v2 = Helpers.extract_int_from_reg v2 in
	
	(* Run the binary operator *)
	let result = `L_Int (binary_op v1 v2) in
	
	(* Store the result *) 
	Helpers.update_register registers r1 result;

	(* Update the config *)
	update_config (heap, stack) 1
;;

(*
	Handles 'is' operator instructions (is_tab, is_int, is_str).
	Note: True is represented by 1, false by 0.

	@param (heap, stack) - The heap and stack
	@param (reg, reg, reg) - The registers in the instruction
	@param (value -> int) - A lambda of the is operator 
	@ret (heap, stack) The updated heap and stack
*)
let run_is_a_operator (heap, stack) (r1, r2) is_a_op = 
	(* Expand the stack *)
	let (function_name, program_counter, registers, tail) = Helpers.extract_stack_contents stack in 
	
	(* Extract the value *)
	let v = Helpers.get_register_value registers r2 in 
	
	(* Run the is_a operator *)
	let result = `L_Int (is_a_op v) in 

	(* Store the result *)
	Helpers.update_register registers r1 result;
	
	(* Update the config *)
	update_config (heap, stack) 1
;;

(*
	Runs const(reg, value)
	
	@param [config]
	@param [reg * value] (r,v) - The register and value from the instruction.
	@return [config] The updated config
*)
let run_const (heap, stack, registers) (r, v) =
	(* Place the value into the register *) 
	Helpers.update_register registers r v;
	update_config (heap, stack) 1
;;

(*
	Runs mov(reg, reg)

	@param [config]
	@param [reg * reg] 
	@return [config]
*)
let run_mov (heap, stack, registers) (r1, r2) = 
  (* Copy the value from r2 into r1 *)
  let value = Helpers.get_register_value registers r2 in
  Helpers.update_register registers r1 value;
  update_config (heap, stack) 1
;;

(*
	Runs add(reg, reg, reg)

	@param [config]
	@param [reg * reg * reg] (r1, r2, r3) - The registers from the instruction
	@return [config] The updated config
*)
let run_add (heap, stack) (r1, r2, r3) = 
	let binary_op = fun a b -> a + b in
	run_binary_operator (heap, stack) (r1, r2, r3) binary_op "add"
;;

(*
	Runs sub(reg, reg, reg)

	@param [config]
	@param [reg * reg * reg]
	@return [config]
*)
let run_sub (heap, stack) (r1, r2, r3) = 
	let binary_op = fun a b -> a - b in
	run_binary_operator (heap, stack) (r1, r2, r3) binary_op "sub"
;;

(*
	Runs mul(reg, reg, reg)

	@param [config]
	@param [reg * reg * reg]
	@return [config]
*)
let run_mul (heap, stack) (r1, r2, r3) =  
	let binary_op = fun a b -> a * b in
	run_binary_operator (heap, stack) (r1, r2, r3) binary_op "mul"
;;

(*
	Runs div(reg, reg, reg)

	@param [config]
	@param [reg * reg * reg]
	@return [config]
*)
let run_div (heap, stack) (r1, r2, r3) = 
	let binary_op = fun a b -> a / b in
	run_binary_operator (heap, stack) (r1, r2, r3) binary_op "div"
;;

(*
	Runs lt(reg, reg, reg)

	@param [config]
	@param [reg * reg * reg]
	@return [config]
*)
let run_lt (heap, stack) (r1, r2, r3) = 
	let binary_op = fun a b -> if a < b then 1 else 0 in
	run_binary_operator (heap, stack) (r1, r2, r3) binary_op "lt"
;;

(*
	Runs leq(reg, reg, reg)

	@param [config]
	@param [reg * reg * reg]
	@return [config]
*)
let run_leq (heap, stack) (r1, r2, r3) =  
	let binary_op = fun a b -> if a <= b then 1 else 0 in
	run_binary_operator (heap, stack) (r1, r2, r3) binary_op "leq"
;;

(*
	Runs eq(reg, reg, reg)

	@param [config]
	@param [reg * reg * reg]
	@return [config]
*)
let run_eq (heap, stack, registers) (r1, r2, r3) = 
	let val1 = Helpers.get_register_value registers r2 in 
	let val2 = Helpers.get_register_value registers r3 in 
	let result = if (Helpers.are_registers_equal val1 val2) then 1 else 0 in
	(* Store the result *) 
	Helpers.update_register registers r1 (`L_Int result);
	(* Update the config *)
	update_config (heap, stack) 1
;;

(*
	Runs is_int(reg, reg)

	@param [config]
	@param [reg * reg]
	@return [config]
*)
let run_is_int (heap, stack) (r1, r2) = 
	let is_a_op = ( fun v -> 
		match v with 
			| `L_Int _ -> 1
			| _ -> 0
	) in 
	run_is_a_operator (heap, stack) (r1, r2) is_a_op
;;

(*
	Runs is_str(reg, reg)

	@param [config]
	@param [reg * reg]
	@return [config]
*)
let run_is_str (heap, stack) (r1, r2) = 
	let is_a_op = ( fun v ->
	  match v with 
			| `L_Str _ -> 1
			| _ -> 0
	) in 
	run_is_a_operator (heap, stack) (r1, r2) is_a_op
;;

(*
	Runs is_tab(reg, reg)

	@param [config]
	@param [reg * reg]
	@return [config]
*)
let run_is_tab (heap, stack) (r1, r2) = 
	let is_a_op = ( fun v -> 
		match v with 
			| `L_Loc _ -> 1
			| _ -> 0
	) in 
	run_is_a_operator (heap, stack) (r1, r2) is_a_op
;;

(*
	Runs rd_glob(reg, id)

	@param [config]
	@param [reg * id]
	@return [config]
*)
let run_rd_glob (heap, stack, registers) (r, id) =
	let id = (Helpers.convert_id_to_value id) in 
	if not (Hashtbl.mem heap id) then 
		failwith (
			"Illegal call to rd_glob - The ID " ^ (Helpers.register_to_s id) ^
			", is not bound within the heap"
		)
	;
	let global_var = Hashtbl.find heap id in
	Helpers.update_register registers r global_var;
	update_config (heap, stack) 1
;;

(*
	Runs wr_glob(id, reg)

	@param [config]
	@param [id * reg]
	@return [config]
*)
let run_wr_glob (heap, stack, registers) (id, r) = 
	let v = Helpers.get_register_value registers r in 
	Hashtbl.replace heap (Helpers.convert_id_to_value id) v; 
	update_config (heap, stack) 1
;;

(*
	Runs mk_tab(reg)
*)
let run_mk_tab (heap, stack, registers) r = 
	(* Create a pointer to the table *)
	let loc = `L_Loc (Helpers.next_location ()) in
	(* Create the table *) 
	let new_table = `L_Tab (Hashtbl.create 16) in 
	(* Store the table in the heap *)
	Hashtbl.replace heap loc new_table;
	(* Store the pointer in the register *) 
	Helpers.update_register registers r loc;
	(* Update the config *)
	update_config (heap, stack) 1
;;

(*
	Runs has_tab(reg, reg, reg)
*)
let run_has_tab (heap, stack, registers) (r1, r2, r3) =
	(* Get the pointer to the table *)
	let pointer = Helpers.get_register_value registers r2 in 

	if not (Helpers.is_loc pointer) then 
		failwith (
			"Illegal call to has_tab - Expected to find a Pointer in the 2nd register" ^ 
			", but found: " ^ Helpers.register_to_s pointer
		)
	;

	if not (Hashtbl.mem heap pointer) then 
		failwith (
			"Illegal call to has_tab - The Pointer " ^ (Helpers.register_to_s pointer) ^
			", is not bound within the heap"
		)
	;

	(* Get the table off the heap *)
	let table = Helpers.extract_table_from_reg (Hashtbl.find heap pointer) in 
	(* Get the key to check for *)
	let table_key = Helpers.get_register_value registers r3 in 
	(* Check the table for the key *)
	let result = (if Hashtbl.mem table table_key then 1 else 0) in
	(* Store the result *)
	Helpers.update_register registers r1 (`L_Int result); 
	(* Update the config *)
	update_config (heap, stack) 1
;;

(*
	Runs wr_tab(reg, reg, reg)
*)
let run_wr_tab (heap, stack, registers) (r1, r2, r3) =
	(* Get the pointer *)
	let pointer = Helpers.get_register_value registers r1 in

	if not (Helpers.is_loc pointer) then 
		failwith (
			"Illegal call to wr_tab - Expected to find a Pointer in the 1st register" ^ 
			", but found: " ^ Helpers.register_to_s pointer
		)
	;

	if not (Hashtbl.mem heap pointer) then 
		failwith (
			"Illegal call to wr_tab - The Pointer " ^ (Helpers.register_to_s pointer) ^
			", is not bound within the heap"
		)
	;

	(* Get the table off the heap *)
	let table = Helpers.extract_table_from_reg (Hashtbl.find heap pointer) in 
	(* Retrieve key to write to *)
	let table_key = Helpers.get_register_value registers r2 in 
	(* Get the value to store *)
	let value = Helpers.get_register_value registers r3 in 
	(* Store the value in the table *)
	Hashtbl.replace table table_key value;
	(* Update the table within the heap *)
	Hashtbl.replace heap pointer (`L_Tab table);
	(* Update the config *)
	update_config (heap, stack) 1
;;

(*
	Runs rd_tab(reg, reg, reg)
*)
let run_rd_tab (heap, stack, registers) (r1, r2, r3) = 
	let pointer = Helpers.get_register_value registers r2 in

	if not (Helpers.is_loc pointer) then 
		failwith (
			"Illegal call to rd_tab - Expected to find a Pointer in the 2nd register" ^ 
			", but found: " ^ Helpers.register_to_s pointer
		)
	;

	if not (Hashtbl.mem heap pointer) then 
		failwith (
			"Illegal call to rd_tab - The Pointer " ^ (Helpers.register_to_s pointer) ^
			", is not bound within the heap"
		)
	;
		
	(* Get the table off the heap *)
	let table = Helpers.extract_table_from_reg (Hashtbl.find heap pointer) in 
	(* Get the key from the register *)
	let table_key = Helpers.get_register_value registers r3 in 
	(* Retrieve the value from the table *)
	let table_val = Hashtbl.find table table_key in 
	(* Update the register *)
	Helpers.update_register registers r1 table_val;
	(* Update the config *)
	update_config (heap, stack) 1
;;

(*
	Runs jmp(int)
*)
let run_jmp (heap, stack) n = 
	(* Update the config *)
	update_config (heap, stack) (n + 1)
;;

(*
	Runs if_zero(reg, int)
*)
let run_if_zero (heap, stack, registers) (r, n) =
	let v = Helpers.get_register_value registers r in ( 
		match v with 
			| `L_Int 0 -> update_config (heap, stack) (n + 1)
			| _ -> update_config (heap, stack) 1
	)
;;

(*
	Runs built in function print_string
*)
let run_print_string (heap, stack, instruction) (registers, new_registers) =
	let register_value = Hashtbl.find new_registers 0 in 
	let string_to_print = Helpers.extract_string_from_reg register_value in 
	print_string string_to_print;

	let start_location = Helpers.get_start_location instruction in  
	Hashtbl.replace registers start_location register_value;
	update_config (heap, stack) 1
;;

(*
	Runs built-in function print_int
*)
let run_print_int (heap, stack, instruction) (registers, new_registers) = 
	let register_value = Hashtbl.find new_registers 0 in 
	let int_to_print = Helpers.extract_int_from_reg register_value in 
	print_int int_to_print;

	let start_location = Helpers.get_start_location instruction in  
	Hashtbl.replace registers start_location register_value;
	update_config (heap, stack) 1
;;

(*
	Runs built-in function to_s
*)
let run_to_s (heap, stack, instruction) (registers, new_registers) = 
	let register_value = Hashtbl.find new_registers 0 in 
	let value = ( 
		match register_value with 
			| `L_Str str -> register_value
			| `L_Id id -> `L_Str (String.concat "ID '" [id; "'"] )
			| `L_Int n -> `L_Str (string_of_int n) 
			| _ -> failwith "invalid argument provided to to_s"	
	) in
	let start_location = Helpers.get_start_location instruction in  
	Hashtbl.replace registers start_location value;
	update_config (heap, stack) 1
;; 

(*
	Runs built-in function to_i
*)
let run_to_i (heap, stack, instruction) (registers, new_registers) = 
	let register_value = Hashtbl.find new_registers 0 in
	let value = (
		match register_value with 
			| `L_Str str -> `L_Int (int_of_string str)
			| `L_Int _ -> register_value
			| _ -> failwith "invalid argument provided to to_i"
	) in 
	let start_location = Helpers.get_start_location instruction in  
	Hashtbl.replace registers start_location value;
	update_config (heap, stack) 1
;;

(*
	Runs built-in function size
*)
let run_size (heap, stack, instruction) (registers, new_registers) = 
	let register_val = Hashtbl.find new_registers 0 in
	match register_val with 
		| `L_Loc _ -> 
			let table = Helpers.extract_table_from_reg (Hashtbl.find heap register_val) in 
			let size = `L_Int (Hashtbl.length table) in 
			let start_location = Helpers.get_start_location instruction in  
			Hashtbl.replace registers start_location size;
			update_config (heap, stack) 1
	 	| _ ->
	 		failwith "Invalid argument provided to size"
;;

(*
	Runs built-in function length
*)
let run_length (heap, stack, instruction) (registers, new_registers) = 
	let register_val = Hashtbl.find new_registers 0 in
	match register_val with 
		| `L_Str str -> 
			let length = `L_Int (String.length str) in 
			let start_location = Helpers.get_start_location instruction in  
			Hashtbl.replace registers start_location length;
			update_config (heap, stack) 1
		| _ -> 
			failwith "Invalid argument provided to length"
;;

(*
	Runs built-in function concat
*)
let run_concat (heap, stack, instruction) (registers, new_registers) = 
	let val1 = Hashtbl.find new_registers 0 in 
	let val2 = Hashtbl.find new_registers 1 in
	match (val1, val2) with
		| (`L_Str str1, `L_Str str2) ->
			let result = `L_Str (String.concat "" [str1; str2]) in  
			let start_location = Helpers.get_start_location instruction in  
			Hashtbl.replace registers start_location result;
			update_config (heap, stack) 1
		| _ -> 
			failwith "invalid argument(s) provided to concat" 
;;

(*
	Runs built-in function iter
*)
let run_iter (heap, stack, new_registers) = 
 	(* Get the pointer to the table *)
 	let pointer = Hashtbl.find new_registers 0 in 
 	(* Get the table *)
 	let table = Helpers.extract_table_from_reg (Hashtbl.find heap pointer) in 
 	(* Get the key,value pairs from the table *)
 	let key_vals = (Hashtbl.fold (fun k v lst -> (k, v)::lst) table []) in
 	(* Get the function to iterate with *)
 	let function_to_call = Helpers.extract_id_from_reg (Hashtbl.find new_registers 1) in 
 	(* Get the input to the function (other than a key,value pair) *)
 	let input = Hashtbl.find new_registers 2 in 
 	(* Add the function call to the stack for each key,value pair *)
	let new_stack = Helpers.iterate_key_val_pairs key_vals function_to_call input stack in
 	(* Update the config *)
 	(heap, new_stack)
;;


(*
	Runs call(reg, int, int)
*)
let run_call (heap, stack, registers, program, instruction) (r, start_register, end_register) = 
	(* Copy over the specified registers into a new table *)
	let new_registers = Helpers.copy_registers 0 start_register end_register registers (Hashtbl.create 32) in  
	(* Retrieve the function to call *)
	let function_to_call = Helpers.extract_id_from_reg (Helpers.get_register_value registers r) in 
		
	(* If the function is stored in the program, call it *)
	if Hashtbl.mem program function_to_call then (
		let first_frame = (function_to_call, 0, new_registers) in 
		let new_stack = first_frame::stack in 
		(heap, new_stack)
	) 
	(* The function must be built-in or a typo *)
	else begin 
		match function_to_call with 
			| "print_string" -> 
				run_print_string (heap, stack, instruction) (registers, new_registers)

			| "print_int" -> 
				run_print_int (heap, stack, instruction) (registers, new_registers)

			| "to_s" -> 
				run_to_s (heap, stack, instruction) (registers, new_registers)
 
			| "to_i" -> 
				run_to_i (heap, stack, instruction) (registers, new_registers)

			| "concat" -> 
				run_concat (heap, stack, instruction) (registers, new_registers)
 				 
	 		| "length" -> 
	 			run_length (heap, stack, instruction) (registers, new_registers)
 				  
	 		| "size" ->
	 			run_size (heap, stack, instruction) (registers, new_registers)

 			| "iter" ->
 				run_iter (heap, stack, new_registers)

			| _ -> 
				failwith "Invalid function name provided" 
 		end
;;


(*
	Runs ret(reg)
*)
let run_ret (heap, stack, registers, program) r = 
	match stack with 
		| _::(prev_function_name, prev_program_counter, prev_registers)::tail -> 
			let prev_function = (Hashtbl.find program prev_function_name) in 
			let prev_instruction = (Array.get prev_function prev_program_counter) in 
			let start_location = Helpers.get_start_location prev_instruction in 
			let value = Helpers.get_register_value registers r in 
			Hashtbl.replace prev_registers start_location value;
			let new_stack = (prev_function_name, prev_program_counter + 1, prev_registers)::tail in 
			(heap, new_stack)
	
		| _ -> 
			failwith "Fatal error: The stack does not contain an instruction to return to."
;;


(*
	Performs the work required by the instruction on the top of the stack, and
	outputs the updated state of the program (the heap and stack).

	@param [prog] program - The EmeraldVM program
	@param [config] heap, stack - The current heap and stack of the program.
	@return [config] The updated heap and stack
*)
let run_inst (program:prog) ((heap, stack):config):config =

	(* Extract the instruction on the top of the stack *)
	let (function_name, program_counter, registers, tail) = Helpers.extract_stack_contents stack in  
	let instruction = Helpers.current_instruction program stack in 

	match instruction with 
	
	(*
		Set constant
			r <- v
	*)
	| I_const (r, v) -> 
		run_const (heap, stack, registers) (r, v)

	(*
		Copy value
			r1 <- r2
	*)
  | I_mov (r1, r2) ->
  	run_mov (heap, stack, registers) (r1, r2)
	
	(*
		Addition 
			r1 <- r2 + r3  
	*)
	| I_add (r1, r2, r3) -> 
		run_add (heap, stack) (r1, r2, r3)
	
	(* 
		Substrction
			r1 <- r2 - r3 
	*)	
	| I_sub (r1, r2, r3) ->
		run_sub (heap, stack) (r1, r2, r3)
	
	(* 
		Multiplication
			r1 <- r2 * r3 
	*)	
	| I_mul (r1, r2, r3) -> 
		run_mul (heap, stack) (r1, r2, r3)
 	
	(* 
		Division
			r1 <- r2 / r3 
	*)		
	| I_div (r1, r2, r3) -> 
		run_div (heap, stack) (r1, r2, r3)
		
	(* 
		Equality 
			r1 <- r2 == r3
	*)	
	| I_eq (r1, r2, r3) -> 
		run_eq (heap, stack, registers) (r1, r2, r3)
		
	(* 
		Less Than
			r1 <- r2 < r3 
	*)								
	| I_lt (r1, r2, r3) ->
		run_lt (heap, stack) (r1, r2, r3)

	(*
		Less than / equal to 
			r1 <- r2 <= r3 
	*)							
	| I_leq (r1, r2, r3) -> 
		run_leq (heap, stack) (r1, r2, r3)
	
	(* 
		Is an integer?
			r1 <- Helpers.is_integer?(r2) 
	*)						
	| I_is_int (r1, r2) ->
		run_is_int (heap, stack) (r1, r2)

	
	(*
		Is a string?
			r1 <- is_string?(r2)
	*)						
	| I_is_str (r1, r2) -> 
		run_is_str (heap, stack) (r1, r2)
	
	(*
		Is a table?
			r1 <- is_table?(r2)
	*)						
	| I_is_tab (r1, r2) -> 
		run_is_tab (heap, stack) (r1, r2)
	
	(* 
		Jump lines
			Moves the program counter n lines ahead.
	*)					
	| I_jmp n -> 
		run_jmp (heap, stack) n 

	(*
		Jump lines if 0
			Moves the program counter n lines ahead if the given value
			is 0; otherwise, moves to the next consecutive instruction.
	*)
	| I_if_zero (r, n) -> 
		run_if_zero (heap, stack, registers) (r, n)
	
	(*
		Read global variable
			r <- heap(x)
	*)						
	| I_rd_glob (r, id) -> 
		run_rd_glob (heap, stack, registers) (r, id)
	
	(*
		Write to global vairable
			heap(x) <- r
	*)
	| I_wr_glob (id, r) -> 
		run_wr_glob (heap, stack, registers) (id, r)
	
	(*
		Create new table	
			p <- new pointer
			heap[p] <- new table
			r <- p
	*)
	| I_mk_tab r -> 
		run_mk_tab (heap, stack, registers) r 
	
	(*
		Read from table
			table <- heap(r2)
			r1 <- table(r3)
	*)
	| I_rd_tab (r1, r2, r3) -> 
		run_rd_tab (heap, stack, registers) (r1, r2, r3)
	
	(*
		Write to table
			table <- heap(r1)
			table(r2) <- r3
	*)							
	| I_wr_tab (r1, r2, r3) -> 
		run_wr_tab (heap, stack, registers) (r1, r2, r3)
						
	
	(*
		Table exist?
			table <- heap(r2)
			r1 <- table.include_key?(r3)
	*)	
	| I_has_tab (r1, r2, r3) -> 
		run_has_tab (heap, stack, registers) (r1, r2, r3)
	
	(*
		Call a method
			method_name <- r 

	*)					
	| I_call (r, start_register, end_register) -> 
		run_call (heap, stack, registers, program, instruction) (r, start_register, end_register)

 	(*
		Return from a function call...
			Retrieve the return value and places it into the registers of the caller.
			The result is placed in the first register specified by the call method. 
 	*)
	| I_ret r -> 
		run_ret (heap, stack, registers, program) r 

	(* You should return a halt value here, not raise an error *)
	| I_halt r -> 
		failwith "Halt Reached"
;;

(*
	This top-level algorithm for running a program. 
	This method recursively moves through the instructions
	of the program until it reaches the return statement in
	main, at which point it extracts the value from the 
	specified register and returns it to the caller.
*)
let rec run_aux (p:prog) (c:config) = 
	try (
		(* Run the current instruction *)
		let conf = (run_inst p c) in

		(* Execute the next instruction or end the program *) 
		match conf with 

			(*
				Special case: We are in the main method.
				If we are at return, it is the end of the program,
				otherwise, we need to keep running instructions
				until we reach return.
			*)
			| (h, ("main", pc, reg)::[]) -> 
				let p_main = (Hashtbl.find p "main") in 
				let next_instr = (Array.get p_main pc) in (
				match next_instr with  
					(* EOP - return the value in the register specified *)
					| I_ret r -> `Reg (Hashtbl.find reg (Helpers.extract_reg_value r))
					(* Keep running the program *)
					| _ -> run_aux p conf
				)

			(* 
				We are in some function, keep running the instructions. 
			*)
			| _ -> run_aux p conf
	)
	(* Handle errors *)
	with Failure explanation -> 
		let (_,s) = c in
		let (_, _, registers, tail) = Helpers.extract_stack_contents s in 
		match (Helpers.current_instruction p s) with 
			| I_halt r -> 
				`Halt (Hashtbl.find registers (Helpers.extract_reg_value r))
			| _ -> 
				failwith explanation
;;


let add_iter_to_program program =
	(*
		Right now you are cheating the implementation of
		return.

		When you run iter, you end up placing several 
		calls onto the stack. The first item on the stack
		is executed normally. However, the rest of the calls
		are not... 
		The return method comes back to the first instruction,
		I_call, thinking it had just performed this call, so it moves the program
		counter up one, which then places you on the I_const statement, and
		thus you never make the call at at all. 

		To circumvent the above problem, you have added in a bogus I_call
		statement so that when the counter is incremented, you land on the
		correct I_call statement, and the iteration occurs. 
	*)
	let instructions = [|
		I_call (`L_Reg 0, 4, 4);
		I_call (`L_Reg 0, 1, 3);
		I_const (`L_Reg 0, `L_Int 0);
		I_ret (`L_Reg 0)
	|] in 
	Hashtbl.replace program ":iter" instructions;

	let instructions = [|
		I_call (`L_Reg 0, 1, 3);
		I_const (`L_Reg 0, `L_Int 0);
		I_ret (`L_Reg 0)
	|] in 
	Hashtbl.replace program ":start_iter" instructions
;;

(*
	Runs the given program
*)		
let run_prog (p:prog) = 
	add_iter_to_program p;
	let heap = (Hashtbl.create 32) in 
	let stack = [("main", 0, (Hashtbl.create 32))] in 
	run_aux p (heap, stack)
;;
