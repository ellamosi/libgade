package Gade.Dev.CPU.Instructions.Logic.Instances is

   --  AND
   procedure AND_A_A is new ALU_A_Source (ALU_AND, SRC_A);
   procedure AND_A_B is new ALU_A_Source (ALU_AND, SRC_B);
   procedure AND_A_C is new ALU_A_Source (ALU_AND, SRC_C);
   procedure AND_A_D is new ALU_A_Source (ALU_AND, SRC_D);
   procedure AND_A_E is new ALU_A_Source (ALU_AND, SRC_E);
   procedure AND_A_H is new ALU_A_Source (ALU_AND, SRC_H);
   procedure AND_A_L is new ALU_A_Source (ALU_AND, SRC_L);
   procedure AND_A_Addr_HL is new ALU_A_Source (ALU_AND, SRC_Addr_HL);
   procedure AND_A_Imm8 is new ALU_A_Source (ALU_AND, SRC_Imm8);

   --  XOR
   procedure XOR_A_A is new ALU_A_Source (ALU_XOR, SRC_A);
   procedure XOR_A_B is new ALU_A_Source (ALU_XOR, SRC_B);
   procedure XOR_A_C is new ALU_A_Source (ALU_XOR, SRC_C);
   procedure XOR_A_D is new ALU_A_Source (ALU_XOR, SRC_D);
   procedure XOR_A_E is new ALU_A_Source (ALU_XOR, SRC_E);
   procedure XOR_A_H is new ALU_A_Source (ALU_XOR, SRC_H);
   procedure XOR_A_L is new ALU_A_Source (ALU_XOR, SRC_L);
   procedure XOR_A_Addr_HL is new ALU_A_Source (ALU_XOR, SRC_Addr_HL);
   procedure XOR_A_Imm8 is new ALU_A_Source (ALU_XOR, SRC_Imm8);

   --  OR
   procedure OR_A_A is new ALU_A_Source (ALU_OR, SRC_A);
   procedure OR_A_B is new ALU_A_Source (ALU_OR, SRC_B);
   procedure OR_A_C is new ALU_A_Source (ALU_OR, SRC_C);
   procedure OR_A_D is new ALU_A_Source (ALU_OR, SRC_D);
   procedure OR_A_E is new ALU_A_Source (ALU_OR, SRC_E);
   procedure OR_A_H is new ALU_A_Source (ALU_OR, SRC_H);
   procedure OR_A_L is new ALU_A_Source (ALU_OR, SRC_L);
   procedure OR_A_Addr_HL is new ALU_A_Source (ALU_OR, SRC_Addr_HL);
   procedure OR_A_Imm8 is new ALU_A_Source (ALU_OR, SRC_Imm8);

   --  CP
   procedure CP_A_A is new ALU_A_Source (ALU_CP, SRC_A);
   procedure CP_A_B is new ALU_A_Source (ALU_CP, SRC_B);
   procedure CP_A_C is new ALU_A_Source (ALU_CP, SRC_C);
   procedure CP_A_D is new ALU_A_Source (ALU_CP, SRC_D);
   procedure CP_A_E is new ALU_A_Source (ALU_CP, SRC_E);
   procedure CP_A_H is new ALU_A_Source (ALU_CP, SRC_H);
   procedure CP_A_L is new ALU_A_Source (ALU_CP, SRC_L);
   procedure CP_A_Addr_HL is new ALU_A_Source (ALU_CP, SRC_Addr_HL);
   procedure CP_A_Imm8 is new ALU_A_Source (ALU_CP, SRC_Imm8);

end Gade.Dev.CPU.Instructions.Logic.Instances;
