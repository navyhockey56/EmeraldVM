(* Do not change this file! *)

open Emeraldvm

let main () =
  let input_file = ref "" in
  Arg.parse [] (fun s -> input_file := s)  "Usage: ./main.byte input_file";
  if !input_file <> "" then
    let chan = open_in (!input_file) in
    let lexbuf = Lexing.from_channel chan in
    let p =
      try Parser.main Lexer.token lexbuf
      with _ -> Printf.printf "Parse error!\n"; exit 0 in
    let p' = Hashtbl.create 17 in
    let _ = List.iter (fun (f, is) -> Hashtbl.add p' f (Array.of_list is)) p in
    let _ = close_in chan in
    (*    Disassembler.disassemble p'; *)
    match Emeraldvm.run_prog p' with
    | `Halt v -> Printf.printf "halt: %a\n" Disassembler.value v
    | `Reg v -> Printf.printf "%a\n" Disassembler.value v
  else
    Printf.printf "Usage: ./main.byte input_file\n"

;;

main ()
