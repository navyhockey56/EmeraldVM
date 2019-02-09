open Emeraldvm

(*
  This is the entry point to the virtual machine.
  The program requires an EmeraldByte program as input.

  Reading the EmeraldByte file:
    First, lexer.mll defines the regular expressions that represent EmeraldByte's 'tokens'.
    A token is just a single unit of the EmeraldByte syntax, such as JMP which 
    represent the invocation of the 'jmp' instruction.
    
    Next, parser.mly receives the tokenized input and converts 
    the tokens into the types defines in instr.ml. To achieve this,
    parser.mly defines groupings of tokens (sometimes in a recursive manner)
    and the type they correspond to. These definitions are then used to
    automatically convert the tokenized input into the types from instr.ml.

    The result of parsing the the program is a list of tuples, where each inner
    tuple contains the instructions of a single function defined within the program,
    along with that functions name.
    
    This list of tuples is then added to a Hashtbl, thus defining the prog type. This
    prog representation is then able to be run by the virtual machine.  
  
  Running the program:
    emeraldvm.ml contains the core code for the virtual machine. To run a program through
    the virual machine, a stack and heap are created for the program. 

    The stack is a list of (string * int * Hashtbl) tuples which correspond to the
    current function name, the program counter (which instruction we are on), and
    the table which holds the values of the registers used.
    
    The heap is just a Hashtbl of (value -> value) types (value types represent the contents of registers). 
    The heap is used to store two things: 
      1. global variables (wr_glob instruction)
      2. tables defined within the EmeraldByte program (mk_tab instruction)

    The virtual machine is quite simple. It determines the current instruction based
    off what's on the top of the stack, performs the work of that instruction (prints
    a string, creates an int, makes a new table, etc) - updating the heap and registers
    table as it needs, then updates the stack so that the next instruction can run 
    (most of the time, this means just incrementing the program counter by 1). The virtual
    machine continues in this manner until is reaches the return instruction in main, reaches
    a halt instruction, or hits a runtime failure (such as a bad table read).    
*)


(* 
  Runs the given program 
  
  @param [Intr.prog] program - The program to run
  @param [ref bool] verbose - Whether to run the program with verbose logging
*)
let run_program program verbose = 
  let seperator = "\n------------------------------------------------------------------------\n" in
  if !verbose then print_string "Starting program" else ();
  if !verbose then print_string seperator else ();

  let result = try Emeraldvm.run_prog program with Failure ex ->
    print_endline ("Runtime Failure: " ^ ex); exit 0 
  in 

  match result with
    | `Halt v -> 
      if !verbose then print_string seperator else ();
      print_string "HALT REACHED: ";
      Printf.printf "%a\n" Disassembler.value v
    | `Reg v -> 
      if !verbose then print_string seperator else ();
      if !verbose then print_string "Program Returned: " else ();
      Printf.printf "%a\n" Disassembler.value v
;;

(*
  Creates a program from the given input file

  @param [ref string] input_file - The path to the input files.

  @return [Instr.prog] The program defined by the file.
*)
let setup_program input_file = 
  (* Open the input file *)
  let chan = open_in (!input_file) in
  let lexbuf = Lexing.from_channel chan in
  
  (* Attempt to parse the file *)
  let p = try 
    Parser.main Lexer.token lexbuf
  with _ -> 
    Printf.printf "Parse error!\n"; exit 0 
  in
  
  (* Create the program *)
  let p' = Hashtbl.create 17 in
  let _ = List.iter (fun (f, is) -> Hashtbl.add p' f (Array.of_list is)) p in

  (* Close the input file *)
  let _ = close_in chan in
  
  (* Return the program *)
  p'
;;

(*
  Entry to program
*)
let main () =
  
  (* Setup reading program arguments *)
  let verbose = ref false in
  let input_file = ref "" in
  let specs = [("-v", Arg.Set verbose, "Enable verbose mode")] in 
  (* Parse the program arguments *)
  Arg.parse specs (fun s -> input_file := s)  "Usage: ./main.byte input_file";

  if !input_file = "" then 
    Printf.printf "Usage: ./main.byte input_file\n"
  else 
    let program = setup_program input_file in 
    run_program program verbose
;;

main ()
