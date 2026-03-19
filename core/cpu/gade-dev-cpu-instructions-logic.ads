package Gade.Dev.CPU.Instructions.Logic is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Do_AND
     (CPU   : in out CPU_Context;
      Value :        Byte);

   procedure Do_OR
     (CPU   : in out CPU_Context;
      Value :        Byte);

   procedure Do_XOR
     (CPU   : in out CPU_Context;
      Value :        Byte);

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

private

   procedure Adjust_Logic_Flags
     (CPU   : in out CPU_Context;
      Set_H :        Boolean);

end Gade.Dev.CPU.Instructions.Logic;
