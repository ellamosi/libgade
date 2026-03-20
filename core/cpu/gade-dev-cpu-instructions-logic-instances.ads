package Gade.Dev.CPU.Instructions.Logic.Instances is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure AND_A_H is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_H);

   procedure XOR_A_L is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_L);

   procedure OR_A_A is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_A);

   procedure CP_A_Addr_HL is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_Addr_HL);

   procedure AND_A_B is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_B);

   procedure AND_A_C is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_C);

   procedure AND_A_D is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_D);

   procedure AND_A_E is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_E);

   procedure AND_A_L is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_L);

   procedure AND_A_Addr_HL is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_Addr_HL);

   procedure AND_A_A is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_A);

   procedure XOR_A_B is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_B);

   procedure XOR_A_C is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_C);

   procedure XOR_A_D is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_D);

   procedure XOR_A_E is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_E);

   procedure XOR_A_H is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_H);

   procedure XOR_A_Addr_HL is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_Addr_HL);

   procedure XOR_A_A is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_A);

   procedure OR_A_B is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_B);

   procedure OR_A_C is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_C);

   procedure OR_A_D is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_D);

   procedure OR_A_E is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_E);

   procedure OR_A_H is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_H);

   procedure OR_A_L is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_L);

   procedure OR_A_Addr_HL is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_Addr_HL);

   procedure CP_A_B is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_B);

   procedure CP_A_C is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_C);

   procedure CP_A_D is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_D);

   procedure CP_A_E is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_E);

   procedure CP_A_H is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_H);

   procedure CP_A_L is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_L);

   procedure CP_A_A is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_A);

   procedure AND_A_Imm8 is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_AND,
      Source    => Instructions.SRC_Imm8);

   procedure XOR_A_Imm8 is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_XOR,
      Source    => Instructions.SRC_Imm8);

   procedure OR_A_Imm8 is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_OR,
      Source    => Instructions.SRC_Imm8);

   procedure CP_A_Imm8 is new Instructions.ALU_A_Source
     (Operation => Instructions.ALU_CP,
      Source    => Instructions.SRC_Imm8);

end Gade.Dev.CPU.Instructions.Logic.Instances;
