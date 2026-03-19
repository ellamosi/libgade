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

   procedure Execute_ADD_HL_BC is new Instructions.Execute_Add_HL
     (Source => Instructions.REG_BC);

   procedure Execute_ADD_HL_DE is new Instructions.Execute_Add_HL
     (Source => Instructions.REG_DE);

   procedure Execute_ADD_HL_HL is new Instructions.Execute_Add_HL
     (Source => Instructions.REG_HL);

   procedure Execute_ADD_HL_SP is new Instructions.Execute_Add_HL
     (Source => Instructions.REG_SP);

   procedure Execute_ADD_SP_Imm8
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_Add_SP_Imm8;

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

   procedure Execute_INC_BC is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_BC);

   procedure Execute_INC_B is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_B);

   procedure Execute_DEC_B is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_B);

   procedure Execute_DEC_BC is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_BC);

   procedure Execute_INC_C is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_C);

   procedure Execute_DEC_C is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_C);

   procedure Execute_INC_DE is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_DE);

   procedure Execute_INC_D is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_D);

   procedure Execute_DEC_D is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_D);

   procedure Execute_DEC_DE is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_DE);

   procedure Execute_INC_E is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_E);

   procedure Execute_DEC_E is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_E);

   procedure Execute_INC_H is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_H);

   procedure Execute_DEC_H is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_H);

   procedure Execute_INC_L is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_L);

   procedure Execute_DEC_L is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_L);

   procedure Execute_INC_SP is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_SP);

   procedure Execute_INC_Addr_HL is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_DEC_Addr_HL is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_INC_HL is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_HL);

   procedure Execute_DEC_HL is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_HL);

   procedure Execute_DEC_SP is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_SP);

   procedure Execute_INC_A is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_A);

   procedure Execute_DEC_A is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_A);

   procedure Execute_DAA
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_DAA;

private

   type Carry_Type is new Boolean;

   ADC_Carry : constant Carry_Type := True;
   SBC_Carry : constant Carry_Type := True;
   ADD_Carry : constant Carry_Type := False;
   SUB_Carry : constant Carry_Type := False;

end Gade.Dev.CPU.Instructions.Arithmetic;
