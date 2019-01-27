(*
  Representation of a register within a rubevm program.
*)
type reg = [ `L_Reg of int ]

(*
  Representation of a value within a rubevm program.
*)
type value = [ 
    `L_Int of int 
  | `L_Str of string 
  | `L_Id of string 
  | `L_Loc of int 
  | `L_Tab of (value, value) Hashtbl.t
]

(*
  Representation of a function pointer or variable
*)
type id = [ `L_Id of string ]

(*
  Representation of the instructions within a rubevm program.
*)
type instr =
  | I_const of reg * value (* dst, src *)
  | I_mov of reg * reg (* dst, src *)
  | I_add of reg * reg * reg (* dst, src1, src2 *)
  | I_sub of reg * reg * reg (* dst, src1, src2 *)
  | I_mul of reg * reg * reg (* dst, src1, src2 *)
  | I_div of reg * reg * reg (* dst, src1, src2 *)
  | I_eq of reg * reg * reg (* dst, src1, src2 *)
  | I_lt of reg * reg * reg (* dst, src1, src2 *)
  | I_leq of reg * reg * reg (* dst, src1, src2 *)
  | I_is_int of reg * reg (* dst, src *) 
  | I_is_str of reg * reg (* dst, src *) 
  | I_is_tab of reg * reg (* dst, src *) 
  | I_jmp of int (* offset *)
  | I_if_zero of reg * int (* src, offset *)
  | I_rd_glob of reg * id (* dst, src *)
  | I_wr_glob of id * reg (* dst, src *)
  | I_mk_tab of reg (* dst *)
  | I_rd_tab of reg * reg * reg (* dst, tab, key *)
  | I_wr_tab of reg * reg * reg (* tab, key, value *)
  | I_has_tab of reg * reg * reg (* dst, tab, key *)
  | I_call of reg * int * int (* dst, first, last *)
  | I_ret of reg (* src *)
  | I_halt of reg (* src *)

(*
  Type: fn 
  The representation of a function/method.
  
  A function is simply an array of instructions.
*)
type fn = instr array

(*
  Type: prog
  The representation of a rubevm program.
  
  A rubevm program is a table of functions, mapped to
  by their name.
*)
type prog = (string, fn) Hashtbl.t

(*
  Type: heap
  The representation of the heap of a rubevm program.

  A rubevm program's heap is simply a hash from value to value.
*)
type heap = (value, value) Hashtbl.t

(*
  Type: regs
  The store of registers in a rubevm program. 

  A rubevm program's registers are stored as a map 
  from the registers number to the value object
  contained within it.
*)
type regs = (int, value) Hashtbl.t

(*
  Type: stack
  The representation of the stack of a rubevm program

  A rubevm's stack is a list (used in stack form) tuples.
  Each tuple holds the state of the program as follows:
    string - The function name
    int - The program counter
    regs - The registers for the program 
*)
type stack = (string * int * regs) list

(*
  Type: config
  The configuration of a running rubevm program.

  A rubevm's running program is determined by it's heap
  and stack.
*)
type config = heap * stack
