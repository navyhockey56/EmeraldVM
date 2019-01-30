## EmeraldVM
EmeraldVM is a virtual machine for the bytecode language EmeraldByte. The purpose of EmeraldVM is to act as the virtual machine for Emerald, a simple imperative language which compiles into EmeraldByte.

### Program Requirements
- OCaml (built with 4.06)
- OCamlbuild
- OCamlfind
- Ocamllex
- Ruby (for running test script only)

### Building the project
To build the project, execute:
```
> ./Makefile
```
Note: You must have OCaml, Ocamlbuild, Ocamlfind, and Ocamllex installed. If you are missing these requirements, it is reccomended to use `opam - https://opam.ocaml.org/doc/Install.html` to install them.

### Running an EmeraldByte program
Running an EmeraldByte file is as simple as:
```
> ./main.byte file_to_run.evm
```
You can also run the virtual machine with some extra logging using `-v`
```
> ./main.byte -v file_to_run.evm
```

### Running the EmeraldVM tests
The ruby test script requires your project structure to be:
```
main.byte
tests/run_tests.rb
tests/inputs/
tests/outputs/
```
The test script will run any file contained within the `tests/inputs/` directory following the naming convention:
- File name starts with `test_`
- File has extension `.evm`

The test results for each test script must be contained within the `tests/outputs/`, and must follow the naming convention:
- File name is the same as the test script, except for the extension
- File has extension `.out`

To run the test script, navigate to the `tests/` directory and execute:
```
> ruby run_tests.rb
```

### Overview of EmeraldByte
The language EmeraldByte contains only a few instructions (detailed below). Registers are specified as `r0`, `r1`, ..., etc. Single line comments are supported through `#`. All EmeraldByte programs must contain a `main:` method, and all methods must end with the `ret` instruction.

### EmeraldByte Instructions

#### const <i>r</i>, <i>N</i>
Place integer <i>N</i> into register <i>r</i>.

#### const <i>r</i>, <i>"STRING"</i>
Place a string into register <i>r</i>.

#### const <i>r</i>, <i>FUNC</i>
Place function id <i>FUNC</i> into register <i>r</i>.

#### mov <i>r0</i>, <i>r1</i>
Copy value from register <i>r1</i> into register <i>r0</i>.

#### add <i>r0</i>, <i>r1</i>, <i>r2</i>
Store the sum of registers <i>r1</i> and <i>r2</i> into register <i>r0</i>.

#### sub <i>r0</i>, <i>r1</i>, <i>r2</i>
Store the difference of registers <i>r1</i> and <i>r2</i> into register <i>r0</i>.

#### div <i>r0</i>, <i>r1</i>, <i>r2</i>
Store the dividend of registers <i>r1</i> by <i>r2</i> into register <i>r0</i>.

#### mul <i>r0</i>, <i>r1</i>, <i>r2</i>
Store the product of registers <i>r1</i> and <i>r2</i> into register <i>r0</i>.

#### lt <i>r0</i>, <i>r1</i>, <i>r2</i>
Determines if the register <i>r1</i> is less than register <i>r2</i> and stores the result in <i>r0</i>.

#### leq <i>r0</i>, <i>r1</i>, <i>r2</i>
Determines if the register <i>r1</i> is less than or equal to register <i>r2</i> and stores the result in <i>r0</i>.

#### eq <i>r0</i>, <i>r1</i>, <i>r2</i>
Determines if the register <i>r1</i> is equal to register <i>r2</i> and stores the result in <i>r0</i>.

#### is_int <i>r0</i>, <i>r1</i>
Determines if register <i>r1</i> is an integer and stores the result in <i>r0</i>.

#### is_string <i>r0</i>, <i>r1</i>
Determines if register <i>r1</i> is a string and stores the result in <i>r0</i>.

#### is_tab <i>r0</i>, <i>r1</i>
Determines if register <i>r1</i> is the location of a table and stores the result in <i>r0</i>.

#### wr_tab <i>r0</i>, <i>r1</i>, <i>r2</i>
Writes the value stored in register <i>r2</i> under the key in register <i>r1</i> into the table located by register <i>r0</i>.

#### rd_tab <i>r0</i>, <i>r1</i>, <i>r2</i>
Writes the value stored under key in register <i>r2</i> within table located by register <i>r1</i> into the register <i>r0</i>.

#### has_tab <i>r0</i>, <i>r1</i>, <i>r2</i>
Determines if the table located by register <i>r1</i> has a mapping for the key in register <i>r2</i> and stores the result in <i>r0</i>.

#### rd_glob <i>r0</i>, <i>id</i>
Writes the global value stored under <i>id</i> into register <i>r0</i>.

#### wr_tab <i>id</i>, <i>r1</i>
Writes the value stored in register <i>r1</i> as a global value identified by <i>id</i>

#### call <i>r</i>, <i>N</i>, <i>M</i>
Calls the method identified by register <i>r</i>. Maps the current registers <i>rN</i>, <i>rN+1</i>, ..., <i>rM</i> into the method's registers <i>r0</i>, <i>r1</i>, ..., <i>rM-N</i>. Stores the return result of the method into register <i>rN</i>.

#### ret <i>r</i>
Returns the value within register <i>r</i> from the current method to the caller.


### EmeraldByte, built in methods
There are several built-in methods within EmeraldByte.

#### to_s
Takes a single parameter and replaces it with its string representation. For strings, the value remains unchanged; for integers, the value is encased in a string; for ids, the value is replaced with "ID '<i>id</i>'".

#### to_i
Takes a single parameter and replaces it with its integer representation. For integers, the value remains unchanged; for strings of integers, the integer within the string is returned; any other input will produce an error.

#### print_int
Takes a single parameter containing an integer and prints it out.

#### print_string
Takes a single parameter containing a string and print it out.

#### concat
Takes two parameters containing strings and returns their concatenated value.

#### length
Takes a single parameter containing a string and returns the length of the string.

#### size
Takes a singe parameter containing the location of a table and returns the number of keys contained within the table.

#### iter
Takes three parameters - the first is an id of a function which takes 3 parameters (key, value, input); the second parameter contains the location of the table to iterate over; and the last parameter contains any arbitrary value to be passed to the function identified by the first parameter. For each (key,value) pair in the specifed table, the pair along with the value from the third parameter will be passed to the function identified by the first parameter in a call statement.

