with Gade.GB;

package Gade.Dev.CPU.Instructions is

   type Byte_Source_Kind is
     (SRC_A,
      SRC_B,
      SRC_C,
      SRC_D,
      SRC_E,
      SRC_H,
      SRC_L,
      SRC_Addr_BC,
      SRC_Addr_DE,
      SRC_Addr_HL,
      SRC_Addr_HL_Inc,
      SRC_Addr_HL_Dec,
      SRC_Addr_Imm16,
      SRC_High_Addr_C,
      SRC_High_Addr_Imm8,
      SRC_Imm8);

   type Byte_Target_Kind is
     (DST_A,
      DST_B,
      DST_C,
      DST_D,
      DST_E,
      DST_H,
      DST_L,
      DST_Addr_BC,
      DST_Addr_DE,
      DST_Addr_HL,
      DST_Addr_HL_Inc,
      DST_Addr_HL_Dec,
      DST_Addr_Imm16,
      DST_High_Addr_C,
      DST_High_Addr_Imm8);

   type ALU_A_Op_Kind is
     (ALU_ADD,
      ALU_ADC,
      ALU_SUB,
      ALU_SBC,
      ALU_AND,
      ALU_XOR,
      ALU_OR,
      ALU_CP);

   type Bit_Op_Kind is (BIT_Test, BIT_Set, BIT_Reset);

   subtype Bit_Index is Natural range 0 .. 7;

   type Inc_Dec_Op_Kind is (OP_INC, OP_DEC);

   type Word_Register_Kind is (REG_AF, REG_BC, REG_DE, REG_HL, REG_SP);

   type Word_Source_Kind is
     (WSRC_BC,
      WSRC_DE,
      WSRC_HL,
      WSRC_SP,
      WSRC_Imm16);

   type Rotate_Shift_Op_Kind is
     (ROT_RLC,
      ROT_RRC,
      ROT_RL,
      ROT_RR,
      ROT_SLA,
      ROT_SRA,
      ROT_SWAP,
      ROT_SRL);

   type Flow_Op_Kind is
     (FLOW_JR,
      FLOW_JP,
      FLOW_CALL,
      FLOW_RET,
      FLOW_RETI,
      FLOW_RST);

   type Jump_Condition_Kind is
     (JCOND_None,
      JCOND_NZ,
      JCOND_Z,
      JCOND_NC,
      JCOND_C);

   type Jump_Target_Kind is (JTARGET_Imm16, JTARGET_HL);

   generic
      Operation : ALU_A_Op_Kind;
      Source    : Byte_Source_Kind;
   procedure Execute_ALU_A_Source
     (GB : in out Gade.GB.GB_Type);

   generic
      Operation : Bit_Op_Kind;
      Index     : Bit_Index;
      Target    : Byte_Source_Kind;
   procedure Execute_Bit_Source
     (GB : in out Gade.GB.GB_Type);

   generic
      Dest   : Byte_Target_Kind;
      Source : Byte_Source_Kind;
   procedure Execute_LD_Byte
     (GB : in out Gade.GB.GB_Type);

   generic
      Dest   : Word_Register_Kind;
      Source : Word_Source_Kind;
   procedure Execute_LD_Word
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_Imm16_SP
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_HL_SP_Plus_Imm8
     (GB : in out Gade.GB.GB_Type);

   generic
      Operation : Inc_Dec_Op_Kind;
      Target    : Byte_Target_Kind;
   procedure Execute_Inc_Dec_Byte
     (GB : in out Gade.GB.GB_Type);

   generic
      Operation : Inc_Dec_Op_Kind;
      Target    : Word_Register_Kind;
   procedure Execute_Inc_Dec_Word
     (GB : in out Gade.GB.GB_Type);

   generic
      Operation    : Rotate_Shift_Op_Kind;
      Target       : Byte_Target_Kind;
      Adjust_Flags : Boolean := True;
   procedure Execute_Rotate_Shift
     (GB : in out Gade.GB.GB_Type);

   generic
      Source : Word_Register_Kind;
   procedure Execute_Push
     (GB : in out Gade.GB.GB_Type);

   generic
      Dest : Word_Register_Kind;
   procedure Execute_Pop
     (GB : in out Gade.GB.GB_Type);

   generic
      Operation : Flow_Op_Kind;
      Condition : Jump_Condition_Kind := JCOND_None;
      Target    : Jump_Target_Kind := JTARGET_Imm16;
      Vector    : Word := 0;
   procedure Execute_Flow
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_NOP
     (GB : in out Gade.GB.GB_Type);

   generic
      Source : Word_Register_Kind;
   procedure Execute_Add_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_Add_SP_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DAA
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_CPL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_SCF
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_CCF
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_HALT
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_STOP
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DI
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_EI
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions;
