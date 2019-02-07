open Instr
open Helpers

type prog_ret = [ `Reg of value | `Halt of value ]

(*
	Increments the program counter within the top frame of the stack by
	the specified value.

	@param (heap, stack) - The heap and stack
	@param (int) increase_program_counter_by - The amount to increase the program counter by
	@return (heap, stack) The heap and updated stack.
*)
let update_config (heap, stack) increase_program_counter_by = 
	let (function_name, program_counter, registers, tail) = Helpers.extract_stack_contents stack in 
	let new_program_counter = program_counter + increase_program_counter_by in 
	let new_stack = (function_name, new_program_counter, registers)::tail in 
	(heap, new_stack)
;;

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
	Creates an iterations stack frame for the built-in iter method.

	@param [value] key -  A key in the table from the iter call
	@param [value] value - The value mapped to by the key
	@param [string] function_to_call - The string from the `L_Id of the function to call
	@param [value] input - The value passed to every iteration in iter.
	@param [bool] is_first_call - Whether or not this is the first frame in the iter stack.
	@return (string * int * regs) A stack frame for the call to iter.
*)
let iterate_key_val_pair key value function_to_call input is_first_call = 
	(* Create a new set of registers *)
	let new_registers = Hashtbl.create 32 in

	(* 
		Place the function_to_call, key, value, and input into the first 4 registers 
		so that they can be accessed by :start_iter or :iter
	*)
	Hashtbl.replace new_registers 0 (`L_Id function_to_call); 
	Hashtbl.replace new_registers 1 key; 
	Hashtbl.replace new_registers 2 value;
	Hashtbl.replace new_registers 3 input;

	(* Determine the hidden function to call *)
	let function_name = if is_first_call then ":start_iter" else ":iter" in
	(* Create the stack frame *)
	(function_name, 0, new_registers)
;;

(*
	Creates the stack frames for each (key,value) pair in key_vals for the iter method, 
	and adds them to the top of the stack

	@param [(value * value) list] key_vals - The list of (key, value) paris
	@param [string] function_to_call - The function to call for iter
	@param [value] input - The input passed to all iter frames
	@param [stack] stack - the stack
	@param [bool] is_first_call - Whether or not this is the first frame for iter
	@return [stack] The updated stack 
*)
let rec iterate_key_val_pairs_aux key_vals function_to_call input stack is_first_call = 
	match key_vals with 
		| [] -> stack
		| (key, value)::tail -> 
			let new_stack_frame = iterate_key_val_pair key value function_to_call input is_first_call in 
			new_stack_frame::(iterate_key_val_pairs_aux tail function_to_call input stack false)
;;

(*
	
*)
let iterate_key_val_pairs key_vals function_to_call input stack = 
	iterate_key_val_pairs_aux key_vals function_to_call input stack true 
;;

let rec run_inst (program:prog) ((heap, stack):config):config =

	(* Extract the instruction on the top of the stack *)
	let (function_name, program_counter, registers, tail) = Helpers.extract_stack_contents stack in  
	let instruction = Helpers.current_instruction program stack in 
	(*
	Printf.printf "\nInstruction:\t%a\n" Disassembler.dis_instr instruction;
	Disassembler.print_stack stack; 
	print_string "Output: \n\t"; 
	*)	
	match instruction with 
	
	(*
		Set constant
			r <- v
	*)
	| I_const (r, v) -> 
		(* Place the value into the register *) 
		Helpers.update_register registers r v;
		(* Update the stack and return the config *) 
		let stack = ((function_name, (program_counter + 1), registers)::tail) in 
		(heap, stack)

	(*
		Copy value
			r1 <- r2
	*)
  | I_mov (r1, r2) ->
  	(* Copy the value from r2 into r1 *)
  	let value = Helpers.get_register_value registers r2 in
  	Helpers.update_register registers r1 value;
  	(* Update the stack and return the config *) 
		let stack = ((function_name, (program_counter + 1), registers)::tail) in 
		(heap, stack)
	
	(*
		Addition 
			r1 <- r2 + r3  
	*)
	| I_add (r1, r2, r3) -> 
		let binary_op = fun a b -> a + b in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op "add"
	
	(* 
		Substrction
			r1 <- r2 - r3 
	*)	
	| I_sub (r1, r2, r3) ->
		let binary_op = fun a b -> a - b in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op "sub"
	
	(* 
		Multiplication
			r1 <- r2 * r3 
	*)	
	| I_mul (r1, r2, r3) -> 
		let binary_op = fun a b -> a * b in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op "mul"
	
	(* 
		Division
			r1 <- r2 / r3 
	*)		
	| I_div (r1, r2, r3) -> 
		let binary_op = fun a b -> a / b in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op "div"
		
	(* 
		Equality 
			r1 <- r2 == r3
	*)	
	| I_eq (r1, r2, r3) -> 
		let val1 = Helpers.get_register_value registers r2 in 
		let val2 = Helpers.get_register_value registers r3 in 
		let result = if (Helpers.are_registers_equal val1 val2) then 1 else 0 in
		(* Store the result *) 
		Helpers.update_register registers r1 (`L_Int result);
		(* Update the config *)
		update_config (heap, stack) 1
		


	(* 
		Less Than
			r1 <- r2 < r3 
	*)								
	| I_lt (r1, r2, r3) ->
		let binary_op = fun a b -> if a < b then 1 else 0 in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op "lt"

	(*
		Less than / equal to 
			r1 <- r2 <= r3 
	*)							
	| I_leq (r1, r2, r3) -> 
		let binary_op = fun a b -> if a <= b then 1 else 0 in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op "leq"
	
	(* 
		Is an integer?
			r1 <- Helpers.is_integer?(r2) 
	*)						
	| I_is_int (r1, r2) ->
		let is_a_op = ( fun v -> 
			match v with 
			| `L_Int _ -> 1
			| _ -> 0
		) in 
		run_is_a_operator (heap, stack) (r1, r2) is_a_op

	
	(*
		Is a string?
			r1 <- is_string?(r2)
	*)						
	| I_is_str (r1, r2) -> 
		let is_a_op = ( fun v ->
		  match v with 
			| `L_Str _ -> 1
			| _ -> 0
		) in 
		run_is_a_operator (heap, stack) (r1, r2) is_a_op
	
	(*
		Is a table?
			r1 <- is_table?(r2)
	*)						
	| I_is_tab (r1, r2) -> 
		let is_a_op = ( fun v -> 
			match v with 
			| `L_Loc _ -> 1
			| _ -> 0
		) in 
		run_is_a_operator (heap, stack) (r1, r2) is_a_op
	
	(* 
		Jump lines
			Moves the program counter n lines ahead.
	*)					
	| I_jmp n -> 
		(* Update the config *)
		update_config (heap, stack) (n + 1)

	(*
		Jump lines if 0
			Moves the program counter n lines ahead if the given value
			is 0; otherwise, moves to the next consecutive instruction.
	*)
	| I_if_zero (r, n) -> 
		let v = Helpers.get_register_value registers r in ( 
			match v with 
			| `L_Int 0 -> update_config (heap, stack) (n + 1)
			| _ -> update_config (heap, stack) 1
		)
	
	(*
		Read global variable
			r <- heap(x)
	*)						
	| I_rd_glob (r, x) -> 
		let id = (Helpers.convert_id_to_value x) in 
		if not (Hashtbl.mem heap id) then 
			failwith (
				"Illegal call to rd_glob - The ID " ^ (Helpers.register_to_s id) ^
				", is not bound within the heap"
			)
		;
		let global_var = Hashtbl.find heap id in
		Helpers.update_register registers r global_var;
		update_config (heap, stack) 1
	
	(*
		Write to global vairable
			heap(x) <- r
	*)
	| I_wr_glob (x, r) -> 
		let v = Helpers.get_register_value registers r in 
		Hashtbl.replace heap (Helpers.convert_id_to_value x) v; 
		update_config (heap, stack) 1
	
	(*
		Create new table	
			p <- new pointer
			heap[p] <- new table
			r <- p
	*)
	| I_mk_tab r -> 
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
	
	(*
		Read from table
			table <- heap(r2)
			r1 <- table(r3)
	*)
	| I_rd_tab (r1, r2, r3) -> 

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
	
	(*
		Write to table
			table <- heap(r1)
			table(r2) <- r3
	*)							
	| I_wr_tab (r1, r2, r3) -> 
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
						
	
	(*
		Table exist?
			table <- heap(r2)
			r1 <- table.include_key?(r3)
	*)	
	| I_has_tab (r1, r2, r3) -> 
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
	
	(*
		Call a method
			method_name <- r 

	*)					
	| I_call (r, start_register, end_register) -> 
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
					let register_value = Hashtbl.find new_registers 0 in 
					let string_to_print = Helpers.extract_string_from_reg register_value in 
					print_string string_to_print;

					let start_location = Helpers.get_start_location instruction in  
					Hashtbl.replace registers start_location register_value;
					update_config (heap, stack) 1

				| "print_int" -> 
					let register_value = Hashtbl.find new_registers 0 in 
					let int_to_print = Helpers.extract_int_from_reg register_value in 
					print_int int_to_print;

					let start_location = Helpers.get_start_location instruction in  
					Hashtbl.replace registers start_location register_value;
					update_config (heap, stack) 1

				| "to_s" -> 
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
 
	 			| "to_i" -> 
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

	 			| "concat" -> 
	 				let val1 = Hashtbl.find new_registers 0 in 
	 				let val2 = Hashtbl.find new_registers 1 in (
	 					match (val1, val2) with
	 						| (`L_Str str1, `L_Str str2) ->
	 							let result = `L_Str (String.concat "" [str1; str2]) in  
	 							let start_location = Helpers.get_start_location instruction in  
								Hashtbl.replace registers start_location result;
	 							update_config (heap, stack) 1
	 						| _ -> 
	 							failwith "invalid argument(s) provided to concat" 
	 				)
 				 
	 			| "length" -> 
	 				let register_val = Hashtbl.find new_registers 0 in (
	 					match register_val with 
		 					| `L_Str str -> 
		 						let length = `L_Int (String.length str) in 
		 						let start_location = Helpers.get_start_location instruction in  
								Hashtbl.replace registers start_location length;
		 						update_config (heap, stack) 1
		 					| _ -> 
		 						failwith "Invalid argument provided to length"
	 				)
 				  
	 			| "size" ->
	 				let register_val = Hashtbl.find new_registers 0 in (
	 					match register_val with 
	 						| `L_Loc _ -> 
	 							let table = Helpers.extract_table_from_reg (Hashtbl.find heap register_val) in 
	 							let size = `L_Int (Hashtbl.length table) in 
	 							let start_location = Helpers.get_start_location instruction in  
								Hashtbl.replace registers start_location size;
	 							update_config (heap, stack) 1
	 					 	| _ ->
	 					 		failwith "Invalid argument provided to size"
	 				)	

 				| "iter" ->
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
					let new_stack = iterate_key_val_pairs key_vals function_to_call input stack in
 					(* Update the config *)
 					(heap, new_stack)

 				| _ -> 
 					failwith "Invalid function name provided" 

 		end

 	(*
		Return from a function call...
			Retrieve the return value and places it into the registers of the caller.
			The result is placed in the first register specified by the call method. 
 	*)
	| I_ret r -> ( 
		match stack with 
			| _::(prev_function_name, prev_program_counter, prev_registers)::tail -> 
				let prev_function = (Hashtbl.find program prev_function_name) in 
				let prev_instruction = (Array.get prev_function prev_program_counter) in 
				let start_location = Helpers.get_start_location prev_instruction in 
				let value = Helpers.get_register_value registers r in 
				Hashtbl.replace prev_registers start_location value;
				let new_stack = (prev_function_name, prev_program_counter + 1, prev_registers)::tail in 
				(heap, new_stack)
			| _ -> failwith "Fatal error: The stack does not contain an instruction to return to."
	)

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
