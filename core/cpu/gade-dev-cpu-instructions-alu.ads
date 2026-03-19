with Gade.GB;

package Gade.Dev.CPU.Instructions.ALU is
   package Instructions renames Gade.Dev.CPU.Instructions;

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

   procedure Execute_AND_A_H is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_H);

   procedure Execute_XOR_A_L is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_L);

   procedure Execute_OR_A_A is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_A);

   procedure Execute_CP_A_Addr_HL is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_Addr_HL);

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

   procedure Execute_AND_A_B is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_B);

   procedure Execute_AND_A_C is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_C);

   procedure Execute_AND_A_D is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_D);

   procedure Execute_AND_A_E is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_E);

   procedure Execute_AND_A_L is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_L);

   procedure Execute_AND_A_Addr_HL is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_Addr_HL);

   procedure Execute_AND_A_A is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_A);

   procedure Execute_XOR_A_B is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_B);

   procedure Execute_XOR_A_C is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_C);

   procedure Execute_XOR_A_D is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_D);

   procedure Execute_XOR_A_E is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_E);

   procedure Execute_XOR_A_H is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_H);

   procedure Execute_XOR_A_Addr_HL is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_Addr_HL);

   procedure Execute_XOR_A_A is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_A);

   procedure Execute_OR_A_B is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_B);

   procedure Execute_OR_A_C is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_C);

   procedure Execute_OR_A_D is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_D);

   procedure Execute_OR_A_E is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_E);

   procedure Execute_OR_A_H is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_H);

   procedure Execute_OR_A_L is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_L);

   procedure Execute_OR_A_Addr_HL is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_Addr_HL);

   procedure Execute_CP_A_B is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_B);

   procedure Execute_CP_A_C is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_C);

   procedure Execute_CP_A_D is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_D);

   procedure Execute_CP_A_E is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_E);

   procedure Execute_CP_A_H is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_H);

   procedure Execute_CP_A_L is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_L);

   procedure Execute_CP_A_A is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_CP,
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

   procedure Execute_AND_A_Imm8 is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_Imm8);

   procedure Execute_XOR_A_Imm8 is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_Imm8);

   procedure Execute_OR_A_Imm8 is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_Imm8);

   procedure Execute_CP_A_Imm8 is new Instructions.Execute_ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_Imm8);

end Gade.Dev.CPU.Instructions.ALU;
