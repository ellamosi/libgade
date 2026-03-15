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
      Cycles      : M_Cycle_Count := 0;
      Jump_Cycles : M_Cycle_Count := 0;
   end record;

   function Decode
     (GB : in out Gade.GB.GB_Type) return Decoded_Instruction;

end Gade.Dev.CPU.Decode;
