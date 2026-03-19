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

   function Bus_Read_Byte
     (GB      : in out Gade.GB.GB_Type;
      Address :        Word) return Byte;

   procedure Bus_Write_Byte
     (GB      : in out Gade.GB.GB_Type;
      Address :        Word;
      Value   :        Byte);

   procedure Internal_Cycle
     (GB : in out Gade.GB.GB_Type);

   function Fetch_Source
     (GB     : in out Gade.GB.GB_Type;
      Source :        Byte_Source_Kind) return Byte;

   procedure Store_Target
     (GB     : in out Gade.GB.GB_Type;
      Target :        Byte_Target_Kind;
      Value  :        Byte);

   function Load_Target
     (GB     : in out Gade.GB.GB_Type;
      Target :        Byte_Target_Kind) return Byte;

   function Fetch_Imm8
     (GB : in out Gade.GB.GB_Type) return Byte;

   function Fetch_Imm16
     (GB : in out Gade.GB.GB_Type) return Word;

   function Read_Word_Register
     (GB     : Gade.GB.GB_Type;
      Target : Word_Register_Kind) return Word;

   function Read_Word_Source
     (GB     : in out Gade.GB.GB_Type;
      Source :        Word_Source_Kind) return Word;

   procedure Write_Word_Register
     (GB     : in out Gade.GB.GB_Type;
      Target :        Word_Register_Kind;
      Value  :        Word);

   procedure Push_Word
     (GB    : in out Gade.GB.GB_Type;
      Value :        Word);

   procedure Pop_Word
     (GB    : in out Gade.GB.GB_Type;
      Value :    out Word);

   procedure Adjust_HL_Auto
     (GB     : in out Gade.GB.GB_Type;
      Source :        Byte_Source_Kind);

   procedure Adjust_HL_Auto
     (GB     : in out Gade.GB.GB_Type;
      Target :        Byte_Target_Kind);

   generic
      Operation : ALU_A_Op_Kind;
      Source    : Byte_Source_Kind;
   procedure Execute_ALU_A_Source
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions;
