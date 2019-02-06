open Emeraldvm

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
