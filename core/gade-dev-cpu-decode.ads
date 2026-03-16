limited with Gade.GB;

package Gade.Dev.CPU.Decode is

   type Condition_Kind is
     (COND_None, --  Unconditional
      COND_NZ,   --  Zero flag clear
      COND_Z,    --  Zero flag set
      COND_NC,   --  Carry flag clear
      COND_C);   --  Carry flag set

   type Operand_Kind is
     (OD_None,           --  No operand
      OD_A,              --  8-bit register A
      OD_B,              --  8-bit register B
      OD_C,              --  8-bit register C
      OD_D,              --  8-bit register D
      OD_E,              --  8-bit register E
      OD_H,              --  8-bit register H
      OD_L,              --  8-bit register L
      OD_AF,             --  16-bit register pair AF
      OD_BC,             --  16-bit register pair BC
      OD_DE,             --  16-bit register pair DE
      OD_HL,             --  16-bit register pair HL
      OD_SP,             --  Stack pointer
      OD_PC,             --  Program counter
      OD_Addr_BC,        --  Memory at address in BC
      OD_Addr_DE,        --  Memory at address in DE
      OD_Addr_HL,        --  Memory at address in HL
      OD_Addr_HL_Inc,    --  Memory at HL, then increment HL
      OD_Addr_HL_Dec,    --  Memory at HL, then decrement HL
      OD_Addr_Imm16,     --  Memory at 16-bit immediate address
      OD_High_Addr_C,    --  Memory at FF00 + C
      OD_High_Addr_Imm8, --  Memory at FF00 + immediate byte
      OD_Imm8,           --  8-bit immediate operand
      OD_Imm16,          --  16-bit immediate operand
      OD_Rel8,           --  8-bit relative branch displacement
      OD_SP_Plus_Rel8,   --  SP plus signed 8-bit displacement
      OD_Bit_Index,      --  Bit index used by BIT/RES/SET
      OD_RST_Vector);    --  Fixed restart vector

   type Operation_Kind is
     (OP_Invalid, --  Invalid or unimplemented opcode
      OP_NOP,     --  No operation
      OP_LD,      --  Load or store data
      OP_PUSH,    --  Push 16-bit value onto stack
      OP_POP,     --  Pop 16-bit value from stack
      OP_ADD,     --  Add
      OP_ADC,     --  Add with carry
      OP_SUB,     --  Subtract
      OP_SBC,     --  Subtract with carry
      OP_AND,     --  Bitwise AND
      OP_XOR,     --  Bitwise XOR
      OP_OR,      --  Bitwise OR
      OP_CP,      --  Compare against A
      OP_INC,     --  Increment
      OP_DEC,     --  Decrement
      OP_DAA,     --  Decimal adjust accumulator
      OP_CPL,     --  Complement accumulator
      OP_CCF,     --  Complement carry flag
      OP_SCF,     --  Set carry flag
      OP_JR,      --  Relative jump
      OP_JP,      --  Absolute jump
      OP_CALL,    --  Call subroutine
      OP_RET,     --  Return from subroutine
      OP_RETI,    --  Return and enable interrupts
      OP_RST,     --  Restart to fixed vector
      OP_BIT,     --  Test bit
      OP_SET,     --  Set bit
      OP_RES,     --  Reset bit
      OP_RLC,     --  Rotate left circular
      OP_RRC,     --  Rotate right circular
      OP_RL,      --  Rotate left through carry
      OP_RR,      --  Rotate right through carry
      OP_RLCA,    --  Accumulator rotate left circular
      OP_RRCA,    --  Accumulator rotate right circular
      OP_RLA,     --  Accumulator rotate left through carry
      OP_RRA,     --  Accumulator rotate right through carry
      OP_SLA,     --  Shift left arithmetic
      OP_SRA,     --  Shift right arithmetic
      OP_SWAP,    --  Swap high and low nibbles
      OP_SRL,     --  Shift right logical
      OP_DI,      --  Disable interrupts
      OP_EI,      --  Enable interrupts
      OP_HALT,    --  Halt CPU until wake condition
      OP_STOP);   --  Stop CPU

   type Prefix_Kind is (Main, CB);

   type Decoded_Instruction is record
      Prefix      : Prefix_Kind := Main;
      Opcode      : Byte := 0;
      Length      : Positive range 1 .. 3 := 1;
      Operation   : Operation_Kind := OP_Invalid;
      Dest        : Operand_Kind := OD_None;
      Src         : Operand_Kind := OD_None;
      Condition   : Condition_Kind := COND_None;
      Imm8        : Byte := 0;
      Imm16       : Word := 0;
      Rel8        : Signed_Byte := 0;
      Bit_Index   : Natural range 0 .. 7 := 0;
      RST_Vector  : Word := 0;
   end record;

   function Decode
     (GB : in out Gade.GB.GB_Type) return Decoded_Instruction;

   function Decode_Template
     (Prefix : Prefix_Kind;
      Opcode : Byte) return Decoded_Instruction;

private

   --
   --  https://archive.gbdev.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
   --  Bits in opcode | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 | (MSB → LSB)
   --                 |   x   |     y     |     z     |
   --                         |   p   | q |

   --  Upon establishing the opcode, the Z80's path of action is generally dictated by these values:

   --  x = the opcode's 1st octal digit (i.e. bits 7-6)
   --  y = the opcode's 2nd octal digit (i.e. bits 5-3)
   --  z = the opcode's 3rd octal digit (i.e. bits 2-0)
   --  p = y rightshifted one position (i.e. bits 5-4)
   --  q = y modulo 2 (i.e. bit 3)

   --  The following placeholders for instructions and operands are used:

   --  d = displacement byte (8-bit signed integer)
   --  n = 8-bit immediate operand (unsigned integer)
   --  nn = 16-bit immediate operand (unsigned integer)
   --  tab[x] = whatever is contained in the table named tab at index x (analogous for y and z and other table names)

end Gade.Dev.CPU.Decode;
