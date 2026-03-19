with Gade.GB;

package Gade.Dev.CPU.Instructions.Arithmetic is
   package Instructions renames Gade.Dev.CPU.Instructions;

   type Carry_Type is private;

   ADC_Carry : constant Carry_Type;
   ADD_Carry : constant Carry_Type;
   SUB_Carry : constant Carry_Type;
   SBC_Carry : constant Carry_Type;

   procedure Do_Add
     (CPU    : in out CPU_Context;
      Value  :        Byte;
      Result :    out Byte;
      Carry  :        Carry_Type);

   procedure Do_Add
     (CPU    : in out CPU_Context;
      Value  :        Word;
      Result :    out Word);

   procedure Do_Add
     (CPU    : in out CPU_Context;
      Reg    : in out Word;
      Value  :        Byte);

   procedure Do_Sub
     (CPU    : in out CPU_Context;
      Value  :        Byte;
      Result :    out Byte;
      Carry  :        Carry_Type);

   procedure Add_Offset
     (CPU       : in out CPU_Context;
      Address   : in out Word;
      Offset    :        Byte;
      Set_Flags :        Boolean);

   type Inc_Dec_Type is (INC, DEC);

   procedure Do_Inc_Dec
     (CPU     : in out CPU_Context;
      Inc_Dec :        Inc_Dec_Type;
      Value   :        Byte;
      Result  :    out Byte);

   procedure Do_Daa
     (CPU : in out CPU_Context);

   generic
      Operation : Instructions.Inc_Dec_Op_Kind;
      Target    : Instructions.Byte_Target_Kind;
   procedure Execute_Inc_Dec_Byte
     (GB : in out Gade.GB.GB_Type);

   generic
      Operation : Instructions.Inc_Dec_Op_Kind;
      Target    : Instructions.Word_Register_Kind;
   procedure Execute_Inc_Dec_Word
     (GB : in out Gade.GB.GB_Type);

   generic
      Source : Instructions.Word_Register_Kind;
   procedure Execute_Add_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_ADD_A_B is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_B);

   procedure Execute_ADD_A_Addr_HL is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_Addr_HL);

   procedure Execute_ADC_A_C is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_C);

   procedure Execute_SUB_A_D is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_D);

   procedure Execute_SBC_A_E is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_E);

   procedure Execute_ADD_HL_BC
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_ADD_HL_DE
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_ADD_HL_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_ADD_HL_SP
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_ADD_SP_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_ADD_A_C is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_C);

   procedure Execute_ADD_A_D is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_D);

   procedure Execute_ADD_A_E is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_E);

   procedure Execute_ADD_A_H is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_H);

   procedure Execute_ADD_A_L is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_L);

   procedure Execute_ADD_A_A is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_A);

   procedure Execute_ADC_A_B is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_B);

   procedure Execute_ADC_A_D is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_D);

   procedure Execute_ADC_A_E is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_E);

   procedure Execute_ADC_A_H is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_H);

   procedure Execute_ADC_A_L is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_L);

   procedure Execute_ADC_A_Addr_HL is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_Addr_HL);

   procedure Execute_ADC_A_A is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_A);

   procedure Execute_SUB_A_B is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_B);

   procedure Execute_SUB_A_C is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_C);

   procedure Execute_SUB_A_E is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_E);

   procedure Execute_SUB_A_H is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_H);

   procedure Execute_SUB_A_L is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_L);

   procedure Execute_SUB_A_Addr_HL is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_Addr_HL);

   procedure Execute_SUB_A_A is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_A);

   procedure Execute_SBC_A_B is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_B);

   procedure Execute_SBC_A_C is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_C);

   procedure Execute_SBC_A_D is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_D);

   procedure Execute_SBC_A_H is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_H);

   procedure Execute_SBC_A_L is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_L);

   procedure Execute_SBC_A_Addr_HL is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_Addr_HL);

   procedure Execute_SBC_A_A is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_A);

   procedure Execute_ADD_A_Imm8 is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADD,
      Source    => Instructions.SRC_Imm8);

   procedure Execute_ADC_A_Imm8 is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_ADC,
      Source    => Instructions.SRC_Imm8);

   procedure Execute_SUB_A_Imm8 is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SUB,
      Source    => Instructions.SRC_Imm8);

   procedure Execute_SBC_A_Imm8 is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_SBC,
      Source    => Instructions.SRC_Imm8);

   procedure Execute_INC_BC
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_INC_B
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_B
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_BC
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_INC_C
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_C
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_INC_DE
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_INC_D
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_D
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_DE
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_INC_E
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_E
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_INC_H
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_H
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_INC_L
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_L
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_INC_SP
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_INC_Addr_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_Addr_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_INC_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_SP
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_INC_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DEC_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DAA
     (GB : in out Gade.GB.GB_Type);

private

   type Carry_Type is new Boolean;

   ADC_Carry : constant Carry_Type := True;
   SBC_Carry : constant Carry_Type := True;
   ADD_Carry : constant Carry_Type := False;
   SUB_Carry : constant Carry_Type := False;

end Gade.Dev.CPU.Instructions.Arithmetic;
