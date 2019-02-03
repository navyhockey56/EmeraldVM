open Instr

type prog_ret = [ `Reg of value | `Halt of value ]

(*
	Determines the max of two numbers
	@param [Number] 
	@param [Number]
*)    
let max_of a b = 
	if a >= b then a else b
;;

(* 
	Extracts the value from a register
	@param [`L_Reg] The register to extract the value from
*)
let reg_value = function 
	| `L_Reg n -> n
;;

let get_register_value registers register = 
	Hashtbl.find registers (reg_value register)
;;


let update_register registers register new_value =
	Hashtbl.replace registers (reg_value register) new_value
;;



(*
	Determines the maximum register value between the given max, and 
	the registers within the given insruction.
	
	@param [int] max - The previous max
	@param [instr] ins - The instruction to compare the max against.

	@return [int] The determined max value 
*)	
let register_check (max:int) (ins:instr) = match ins with 
	| I_const (i, _) -> max_of max (reg_value i)
	| I_mov (i1, i2) -> max_of (max_of (reg_value i1) (reg_value i2)) max 
	| I_add (i1, i2, i3) -> max_of (max_of (max_of (reg_value i1) (reg_value i2)) (reg_value i3)) max 
	| I_sub (i1, i2, i3) -> max_of (max_of (max_of (reg_value i1) (reg_value i2)) (reg_value i3)) max 
	| I_mul (i1, i2, i3) -> max_of (max_of (max_of (reg_value i1) (reg_value i2)) (reg_value i3)) max 
	| I_div (i1, i2, i3) -> max_of (max_of (max_of (reg_value i1) (reg_value i2)) (reg_value i3)) max 
	| I_eq (i1, i2, i3) -> max_of (max_of (max_of (reg_value i1) (reg_value i2)) (reg_value i3)) max 
	| I_lt (i1, i2, i3) -> max_of (max_of (max_of (reg_value i1) (reg_value i2)) (reg_value i3)) max 
	| I_leq (i1, i2, i3) -> max_of (max_of (max_of (reg_value i1) (reg_value i2)) (reg_value i3)) max  
	| I_is_int (i1, i2) -> max_of (max_of (reg_value i1) (reg_value i2)) max 
	| I_is_str (i1, i2) -> max_of (max_of (reg_value i1) (reg_value i2)) max  
	| I_is_tab (i1, i2) -> max_of (max_of (reg_value i1) (reg_value i2)) max 
	| I_jmp _ -> max
	| I_if_zero (i, _) -> max_of max (reg_value i)
	| I_rd_glob (i, _) -> max_of max (reg_value i)
	| I_wr_glob (_, i) -> max_of max (reg_value i)
	| I_mk_tab (i) -> max_of max (reg_value i)
	| I_rd_tab (i1, i2, i3) -> max_of (max_of (max_of (reg_value i1) (reg_value i2)) (reg_value i3)) max 
	| I_wr_tab (i1, i2, i3) ->  max_of (max_of (max_of (reg_value i1) (reg_value i2)) (reg_value i3)) max 
	| I_has_tab (i1, i2, i3) ->  max_of (max_of (max_of (reg_value i1) (reg_value i2)) (reg_value i3)) max 
	| I_call (i, _, _) -> max_of max (reg_value i)
	| I_ret (i) -> max_of max (reg_value i)
	| I_halt (i) -> max_of max (reg_value i);;

(*
	Moves through the given array of instructions, 
	determining the maximum value of a register contained
	within them. 

	How: Recursively searches through the array, passing
	the previous max, position to check, and the array itself.
	
	@param [int] prev_max - The previous maximum value
	@param [int] position - The position within the array to check
	@param [Array] arr - The array of instructions

	@return [int] The maximum valued register
*)
let rec max_aux (prev_max:int) (position:int) arr = 
	if position >= (Array.length arr) then (prev_max) 
	else (
		let inst = (Array.get arr position) in 
		let max = (register_check prev_max inst) in 
		(max_aux max (position + 1) arr) 
	)
;;

(*
	Determines the max value of a register within the given program.

	@param [prog] p - The program
	@param [string] f - The location of the instructions 
*)	
let max_regs (p:prog) (f:string):int = 
	let arr = (Hashtbl.find p f) in 
	(max_aux 0 0 arr)
;;

(*
	Retrieves the current instruction of the given program
	with the given config.

	@param [prog] p - The program
	@param [config] (heap, stack) - The program configuration

	@return [instr] The current instruction
*)
let current_inst (p:prog) (stack):instr = 
	(* 
		Get the function name and program counter off the
	 	top of the stack, then retrieve the instruction at
	 	the location the program counter specifies.
	 *)
	let (function_name, program_counter, _)::_ = stack in 
	let rubevm_function = Hashtbl.find p function_name in 
	Array.get rubevm_function program_counter
;;

let convert_id_to_value (x:id):value = 
	match x with  
		| `L_Id id -> (`L_Id id)

(*
	Extracts the literal value from a value object.

	@param [value] The value object to extract from

	@return [int | string] The value contained within the value object
*)
let value_of = function
	| `L_Int n -> n
	| `L_Str str -> str
	| `L_Id id -> id
	| `L_Loc n -> n
;;

(*
	Extracts from integer contained within a `L_Int value object.
*)
let integer_reg (r:value) = match r with 
	| `L_Int n -> n
	| _ -> failwith "non-integer value"
;;

(*
	Extracts the id from a `L_Id object
*)	
let id_reg = function 
	| `L_Id id -> id
	| `L_Str s -> failwith (String.concat "L_Str is not a L_Id" [s])
	| `L_Int _ -> failwith "L_Int is not a L_Id"
	| `L_Loc _ -> failwith "L_Loc is not a L_Id"
	| `L_Tab _ -> failwith "L_Tab is not a L_Id"
	| _ -> failwith "non-id value"
;;

(*
	Determines if the two given values are equal.

	@param [value] r1 - The first value
	@param [value] r2 - The second value
 *)
let reg_eq r1 r2 = match (r1,r2) with
	| (`L_Int n1, `L_Int n2) -> n1 = n2
	| (`L_Str str1, `L_Str str2)->  str1 = str2
	| (`L_Id id1, `L_Id id2) -> id1 = id2
	| (`L_Loc n1, `L_Loc n2) -> n2 = n2
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

(*
	Extracts the return value of a return instruction.
*)
let get_r inst = match inst with  
	| I_ret r -> r
	| _ -> failwith "invalid program location - get_r"

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

(*
	Prints out value objects in a human readable format.

	@param [value] The value object to print
*)	
let print_reg = function
	| `L_Int n -> print_string "int "; print_int n
	| `L_Str str ->  print_string "string "; print_string str
	| `L_Id id -> print_string "id "; print_string id
	| `L_Loc n -> print_string "location "; print_int n
	| _ -> print_string "table"
;;


(*
	Extracts the value from a table value

	@param [`L_Tab] tab - The table value

	@return 
*)	
let get_table tab = match tab with 
	| `L_Tab tbl -> tbl 
	| _ -> failwith "not a table";;

(*
	Extracts the string from a string value.

	@param [`L_Str] s - The string value

	@return [string] The contained string
*)	
let get_string s = match s with 
	| `L_Str str -> str 
	| _ -> failwith "not a string";;


let update_config (heap, stack) increase_program_counter_by = 
	let (function_name, program_counter, registers)::tail = stack in 
	let new_program_counter = program_counter + increase_program_counter_by in 
	let new_stack = (function_name, new_program_counter, registers)::tail in 
	(heap, new_stack)
;;
	
let run_binary_operator (heap, stack) (r1, r2, r3) binary_op =
	(* Expand the stack *)
	let (function_name, program_counter, registers)::tail = stack in 
	
	(* Extract the values *)
	let v1 = integer_reg (get_register_value registers r2) in
	let v2 = integer_reg (get_register_value registers r3) in
	
	(* Run the binary operator *)
	let result = `L_Int (binary_op v1 v2) in
	
	(* Store the result *) 
	update_register registers r1 result;

	(* Update the config *)
	update_config (heap, stack) 1
;;

let run_is_a_operator (heap, stack) (r1, r2) is_a_op = 
	(* Expand the stack *)
	let (function_name, program_counter, registers)::tail = stack in 
	
	(* Extract the value *)
	let v = get_register_value registers r2 in 
	
	(* Run the is_a operator *)
	let result = `L_Int (is_a_op v) in 

	(* Store the result *)
	update_register registers r1 result;
	
	(* Update the config *)
	update_config (heap, stack) 1
;;

(*
	This isn't going to work with the way return works...
	You need to wrap these method calls in something that will
	capture the return value and move gracefully to the next iteration.
	
	Right now, when the first return gets hit, you are going to end up
	raising an error when trying to determine the start_location from an
	I_Call instruction that doesn't exist.
*)
let iterate_key_val_pair key value function_to_call input is_first_call = 
	let new_registers = Hashtbl.create 32 in
	Hashtbl.replace new_registers 0 (`L_Id function_to_call); 
	Hashtbl.replace new_registers 1 key; 
	Hashtbl.replace new_registers 2 value;
	Hashtbl.replace new_registers 3 input;
	let function_name = if is_first_call then ":start_iter" else ":iter" in
	(function_name, 0, new_registers)
;;

let rec iterate_key_val_pairs_aux key_vals function_to_call input stack is_first_call = 
	match key_vals with 
		| [] -> stack
		| (key, value)::tail -> 
			let new_stack_frame = iterate_key_val_pair key value function_to_call input is_first_call in 
			new_stack_frame::(iterate_key_val_pairs_aux tail function_to_call input stack false)
;;


let rec iterate_key_val_pairs key_vals function_to_call input stack = 
	iterate_key_val_pairs_aux key_vals function_to_call input stack true 
;;

let rec run_inst (program:prog) ((heap, stack):config):config =

	(* Extract the instruction on the top of the stack *)
	let (function_name, program_counter, registers)::tail = stack in 
	let instruction = current_inst program stack in 
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
		update_register registers r v;
		(* Update the stack and return the config *) 
		let stack = ((function_name, (program_counter + 1), registers)::tail) in 
		(heap, stack)

	(*
		Copy value
			r1 <- r2
	*)
  | I_mov (r1, r2) ->
  	(* Copy the value from r2 into r1 *)
  	let value = get_register_value registers r2 in
  	update_register registers r1 value;
  	(* Update the stack and return the config *) 
		let stack = ((function_name, (program_counter + 1), registers)::tail) in 
		(heap, stack)
	
	(*
		Addition 
			r1 <- r2 + r3  
	*)
	| I_add (r1, r2, r3) -> 
		let binary_op = fun a b -> a + b in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op
	
	(* 
		Substrction
			r1 <- r2 - r3 
	*)	
	| I_sub (r1, r2, r3) ->
		let binary_op = fun a b -> a - b in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op
	
	(* 
		Multiplication
			r1 <- r2 * r3 
	*)	
	| I_mul (r1, r2, r3) -> 
		let binary_op = fun a b -> a * b in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op
	
	(* 
		Division
			r1 <- r2 / r3 
	*)		
	| I_div (r1, r2, r3) -> 
		let binary_op = fun a b -> a / b in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op
		
	(* 
		Equality 
			r1 <- r2 == r3
	*)	
	| I_eq (r1, r2, r3) -> 
		let val1 = get_register_value registers r2 in 
		let val2 = get_register_value registers r3 in 
		let result = if (reg_eq val1 val2) then 1 else 0 in
		(* Store the result *) 
		update_register registers r1 (`L_Int result);
		(* Update the config *)
		update_config (heap, stack) 1
		


	(* 
		Less Than
			r1 <- r2 < r3 
	*)								
	| I_lt (r1, r2, r3) ->
		let binary_op = fun a b -> if a < b then 1 else 0 in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op

	(*
		Less than / equal to 
			r1 <- r2 <= r3 
	*)							
	| I_leq (r1, r2, r3) -> 
		let binary_op = fun a b -> if a <= b then 1 else 0 in
		run_binary_operator (heap, stack) (r1, r2, r3) binary_op
	
	(* 
		Is an integer?
			r1 <- is_integer?(r2) 
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
		let v = get_register_value registers r in ( 
			match v with 
			| `L_Int 0 -> update_config (heap, stack) (n + 1)
			| _ -> update_config (heap, stack) 1
		)
	
	(*
		Read global variable
			r <- heap(x)
	*)						
	| I_rd_glob (r, x) -> 
		let global_var = Hashtbl.find heap (convert_id_to_value x) in
		update_register registers r global_var;
		update_config (heap, stack) 1
	
	(*
		Write to global vairable
			heap(x) <- r
	*)
	| I_wr_glob (x, r) -> 
		let v = get_register_value registers r in 
		Hashtbl.replace heap (convert_id_to_value x) v; 
		update_config (heap, stack) 1
	
	(*
		Create new table	
			p <- new pointer
			heap[p] <- new table
			r <- p
	*)
	| I_mk_tab r -> 
		(* Create a pointer to the table *)
		let loc = `L_Loc (next_location ()) in
		(* Create the table *) 
		let new_table = `L_Tab (Hashtbl.create 16) in 
		(* Store the table in the heap *)
		Hashtbl.replace heap loc new_table;
		(* Store the pointer in the register *) 
		update_register registers r loc;
		(* Update the config *)
		update_config (heap, stack) 1
	
	(*
		Read from table
			table <- heap(r2)
			r1 <- table(r3)
	*)
	| I_rd_tab (r1, r2, r3) -> 
		(* Get the pointer *)
		let pointer = get_register_value registers r2 in (
	 		match pointer with 
			| `L_Loc loc -> 
				(* Get the table off the heap *)
				let table = get_table (Hashtbl.find heap pointer) in 
				(* Get the key from the register *)
				let table_key = get_register_value registers r3 in 
				(* Retrieve the value from the table *)
				let table_val = Hashtbl.find table table_key in 
				(* Update the register *)
				update_register registers r1 table_val;
				(* Update the config *)
				update_config (heap, stack) 1

			| _ -> failwith "r2 is not a location"
	)
	
	(*
		Write to table
			table <- heap(r1)
			table(r2) <- r3
	*)							
	| I_wr_tab (r1, r2, r3) -> 
		(* Get the pointer *)
		let pointer = get_register_value registers r1 in
		(* Get the table off the heap *)
		let table = get_table (Hashtbl.find heap pointer) in 
		(* Retrieve key to write to *)
		let table_key = get_register_value registers r2 in 
		(* Get the value to store *)
		let value = get_register_value registers r3 in 
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
		let pointer = get_register_value registers r2 in 
		(* Get the table off the heap *)
		let table = get_table (Hashtbl.find heap pointer) in 
		(* Get the key to check for *)
		let table_key = get_register_value registers r3 in 
		(* Check the table for the key *)
		let result = (if Hashtbl.mem table table_key then 1 else 0) in
		(* Store the result *)
		update_register registers r1 (`L_Int result); 
		(* Update the config *)
		update_config (heap, stack) 1
	
	(*
		Call a method
			method_name <- r 

	*)					
	| I_call (r, start_register, end_register) -> 
		(* Copy over the specified registers into a new table *)
		let new_registers = copy_registers 0 start_register end_register registers (Hashtbl.create 32) in  
		(* Retrieve the function to call *)
		let function_to_call = id_reg (get_register_value registers r) in 
		
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
					let string_to_print = get_string register_value in 
					print_string string_to_print;

					let start_location = get_start_location instruction in  
					Hashtbl.replace registers start_location register_value;
					update_config (heap, stack) 1

				| "print_int" -> 
					let register_value = Hashtbl.find new_registers 0 in 
					let int_to_print = integer_reg register_value in 
					print_int int_to_print;

					let start_location = get_start_location instruction in  
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
					let start_location = get_start_location instruction in  
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
	 				let start_location = get_start_location instruction in  
					Hashtbl.replace registers start_location value;
	 				update_config (heap, stack) 1

	 			| "concat" -> 
	 				let val1 = Hashtbl.find new_registers 0 in 
	 				let val2 = Hashtbl.find new_registers 1 in (
	 					match (val1, val2) with
	 						| (`L_Str str1, `L_Str str2) ->
	 							let result = `L_Str (String.concat "" [str1; str2]) in  
	 							let start_location = get_start_location instruction in  
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
		 						let start_location = get_start_location instruction in  
								Hashtbl.replace registers start_location length;
		 						update_config (heap, stack) 1
		 					| _ -> 
		 						failwith "Invalid argument provided to length"
	 				)
 				  
	 			| "size" ->
	 				let register_val = Hashtbl.find new_registers 0 in (
	 					match register_val with 
	 						| `L_Loc _ -> 
	 							let table = get_table (Hashtbl.find heap register_val) in 
	 							let size = `L_Int (Hashtbl.length table) in 
	 							let start_location = get_start_location instruction in  
								Hashtbl.replace registers start_location size;
	 							update_config (heap, stack) 1
	 					 	| _ ->
	 					 		failwith "Invalid argument provided to size"
	 				)	

 				| "iter" ->
 					(* Get the pointer to the table *)
 					let pointer = Hashtbl.find new_registers 0 in 
 					(* Get the table *)
 					let table = get_table (Hashtbl.find heap pointer) in 
 					(* Get the key,value pairs from the table *)
 					let key_vals = (Hashtbl.fold (fun k v lst -> (k, v)::lst) table []) in
 					(* Get the function to iterate with *)
 					let function_to_call = id_reg (Hashtbl.find new_registers 1) in 
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
				let start_location = get_start_location prev_instruction in 
				let value = get_register_value registers r in 
				Hashtbl.replace prev_registers start_location value;
				let new_stack = (prev_function_name, prev_program_counter + 1, prev_registers)::tail in 
				(heap, new_stack)
	)

	(* You should return a halt value here, not raise an error *)
	| I_halt r -> 
		raise (Failure "halt reached")	

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
					| I_ret r -> `Reg (Hashtbl.find reg (reg_value r))
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
		let (_,_,reg_file)::_ = s in  
		match (current_inst p s) with 
			| I_halt r -> 
				`Halt (Hashtbl.find reg_file (reg_value r))
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
