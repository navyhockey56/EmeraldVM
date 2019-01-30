## EmeraldVM
EmeraldVM is a virtual machine for the bytecode language EmeraldByte. The purpose of EmeraldVM is to act as the virtual machine for Emerald, a simple imperative language which compiles into EmeraldByte.

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

#### to_i

#### print_int

#### print_string

#### concat

#### length

#### size

#### iter
