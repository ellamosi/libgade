limited with Gade.GB;

package Gade.Dev.CPU.Decode is

   type Condition_Kind is (COND_None, COND_NZ, COND_Z, COND_NC, COND_C);

   type Operand_Kind is
     (OD_None,
      OD_A, OD_B, OD_C, OD_D, OD_E, OD_H, OD_L,
      OD_AF, OD_BC, OD_DE, OD_HL, OD_SP, OD_PC,
      OD_Addr_BC, OD_Addr_DE, OD_Addr_HL,
      OD_Addr_HL_Inc, OD_Addr_HL_Dec,
      OD_Addr_Imm16,
      OD_High_Addr_C,
      OD_High_Addr_Imm8,
      OD_Imm8,
      OD_Imm16,
      OD_Rel8,
      OD_SP_Plus_Rel8,
      OD_Bit_Index,
      OD_RST_Vector);

   type Operation_Kind is
     (OP_Invalid,
      OP_NOP,
      OP_LD,
      OP_PUSH,
      OP_POP,
      OP_ADD,
      OP_ADC,
      OP_SUB,
      OP_SBC,
      OP_AND,
      OP_XOR,
      OP_OR,
      OP_CP,
      OP_INC,
      OP_DEC,
      OP_DAA,
      OP_CPL,
      OP_CCF,
      OP_SCF,
      OP_JR,
      OP_JP,
      OP_CALL,
      OP_RET,
      OP_RETI,
      OP_RST,
      OP_BIT,
      OP_SET,
      OP_RES,
      OP_RLC,
      OP_RRC,
      OP_RL,
      OP_RR,
      OP_RLCA,
      OP_RRCA,
      OP_RLA,
      OP_RRA,
      OP_SLA,
      OP_SRA,
      OP_SWAP,
      OP_SRL,
      OP_DI,
      OP_EI,
      OP_HALT,
      OP_STOP);

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
