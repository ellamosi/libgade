package Gade.Dev.CPU.Instructions.Bitwise is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Execute_BIT_0_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test,
      Index     => 0,
      Target    => Instructions.SRC_B);

   procedure Execute_BIT_3_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test,
      Index     => 3,
      Target    => Instructions.SRC_Addr_HL);

   procedure Execute_RES_0_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_B);
   procedure Execute_RES_0_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_C);
   procedure Execute_RES_0_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_D);
   procedure Execute_RES_0_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_E);
   procedure Execute_RES_0_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_H);
   procedure Execute_RES_0_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_L);
   procedure Execute_RES_0_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_Addr_HL);
   procedure Execute_RES_0_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_A);
   procedure Execute_RES_1_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_B);
   procedure Execute_RES_1_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_C);
   procedure Execute_RES_1_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_D);
   procedure Execute_RES_1_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_E);
   procedure Execute_RES_1_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_H);
   procedure Execute_RES_1_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_L);
   procedure Execute_RES_1_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_Addr_HL);
   procedure Execute_RES_1_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_A);
   procedure Execute_RES_2_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_B);
   procedure Execute_RES_2_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_C);
   procedure Execute_RES_2_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_D);
   procedure Execute_RES_2_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_E);
   procedure Execute_RES_2_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_H);
   procedure Execute_RES_2_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_L);
   procedure Execute_RES_2_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_Addr_HL);
   procedure Execute_RES_2_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_A);
   procedure Execute_RES_3_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_B);
   procedure Execute_RES_3_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_C);
   procedure Execute_RES_3_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_D);
   procedure Execute_RES_3_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_E);
   procedure Execute_RES_3_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_H);
   procedure Execute_RES_3_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_L);
   procedure Execute_RES_3_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_Addr_HL);
   procedure Execute_RES_3_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_A);
   procedure Execute_RES_4_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_B);
   procedure Execute_RES_4_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_C);
   procedure Execute_RES_4_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_D);
   procedure Execute_RES_4_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_E);
   procedure Execute_RES_4_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_H);
   procedure Execute_RES_4_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_L);
   procedure Execute_RES_4_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_Addr_HL);
   procedure Execute_RES_4_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_A);
   procedure Execute_RES_5_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_B);
   procedure Execute_RES_5_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_C);
   procedure Execute_RES_5_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_D);
   procedure Execute_RES_5_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_E);
   procedure Execute_RES_5_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_H);
   procedure Execute_RES_5_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_L);
   procedure Execute_RES_5_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_Addr_HL);
   procedure Execute_RES_5_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_A);
   procedure Execute_RES_6_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_B);
   procedure Execute_RES_6_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_C);
   procedure Execute_RES_6_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_D);
   procedure Execute_RES_6_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_E);
   procedure Execute_RES_6_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_H);
   procedure Execute_RES_6_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_L);
   procedure Execute_RES_6_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_Addr_HL);
   procedure Execute_RES_6_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_A);
   procedure Execute_RES_7_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_B);
   procedure Execute_RES_7_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_C);
   procedure Execute_RES_7_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_D);
   procedure Execute_RES_7_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_E);
   procedure Execute_RES_7_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_H);
   procedure Execute_RES_7_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_L);
   procedure Execute_RES_7_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_Addr_HL);
   procedure Execute_RES_7_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_A);
   procedure Execute_SET_0_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_B);
   procedure Execute_SET_0_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_C);
   procedure Execute_SET_0_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_D);
   procedure Execute_SET_0_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_E);
   procedure Execute_SET_0_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_H);
   procedure Execute_SET_0_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_L);
   procedure Execute_SET_0_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_Addr_HL);
   procedure Execute_SET_0_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_A);
   procedure Execute_SET_1_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_B);
   procedure Execute_SET_1_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_C);
   procedure Execute_SET_1_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_D);
   procedure Execute_SET_1_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_E);
   procedure Execute_SET_1_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_H);
   procedure Execute_SET_1_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_L);
   procedure Execute_SET_1_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_Addr_HL);
   procedure Execute_SET_1_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_A);
   procedure Execute_SET_2_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_B);
   procedure Execute_SET_2_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_C);
   procedure Execute_SET_2_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_D);
   procedure Execute_SET_2_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_E);
   procedure Execute_SET_2_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_H);
   procedure Execute_SET_2_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_L);
   procedure Execute_SET_2_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_Addr_HL);
   procedure Execute_SET_2_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_A);
   procedure Execute_SET_3_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_B);
   procedure Execute_SET_3_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_C);
   procedure Execute_SET_3_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_D);
   procedure Execute_SET_3_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_E);
   procedure Execute_SET_3_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_H);
   procedure Execute_SET_3_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_L);
   procedure Execute_SET_3_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_Addr_HL);
   procedure Execute_SET_3_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_A);
   procedure Execute_SET_4_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_B);
   procedure Execute_SET_4_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_C);
   procedure Execute_SET_4_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_D);
   procedure Execute_SET_4_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_E);
   procedure Execute_SET_4_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_H);
   procedure Execute_SET_4_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_L);
   procedure Execute_SET_4_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_Addr_HL);
   procedure Execute_SET_4_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_A);
   procedure Execute_SET_5_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_B);
   procedure Execute_SET_5_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_C);
   procedure Execute_SET_5_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_D);
   procedure Execute_SET_5_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_E);
   procedure Execute_SET_5_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_H);
   procedure Execute_SET_5_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_L);
   procedure Execute_SET_5_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_Addr_HL);
   procedure Execute_SET_5_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_A);
   procedure Execute_SET_6_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_B);
   procedure Execute_SET_6_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_C);
   procedure Execute_SET_6_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_D);
   procedure Execute_SET_6_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_E);
   procedure Execute_SET_6_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_H);
   procedure Execute_SET_6_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_L);
   procedure Execute_SET_6_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_Addr_HL);
   procedure Execute_SET_6_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_A);
   procedure Execute_SET_7_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_B);
   procedure Execute_SET_7_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_C);
   procedure Execute_SET_7_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_D);
   procedure Execute_SET_7_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_E);
   procedure Execute_SET_7_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_H);
   procedure Execute_SET_7_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_L);
   procedure Execute_SET_7_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_Addr_HL);
   procedure Execute_SET_7_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_A);

   procedure Execute_BIT_0_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_C);
   procedure Execute_BIT_0_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_D);
   procedure Execute_BIT_0_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_E);
   procedure Execute_BIT_0_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_H);
   procedure Execute_BIT_0_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_L);
   procedure Execute_BIT_0_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_Addr_HL);
   procedure Execute_BIT_0_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_A);
   procedure Execute_BIT_1_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_B);
   procedure Execute_BIT_1_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_C);
   procedure Execute_BIT_1_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_D);
   procedure Execute_BIT_1_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_E);
   procedure Execute_BIT_1_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_H);
   procedure Execute_BIT_1_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_L);
   procedure Execute_BIT_1_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_Addr_HL);
   procedure Execute_BIT_1_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_A);
   procedure Execute_BIT_2_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_B);
   procedure Execute_BIT_2_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_C);
   procedure Execute_BIT_2_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_D);
   procedure Execute_BIT_2_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_E);
   procedure Execute_BIT_2_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_H);
   procedure Execute_BIT_2_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_L);
   procedure Execute_BIT_2_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_Addr_HL);
   procedure Execute_BIT_2_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_A);
   procedure Execute_BIT_3_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_B);
   procedure Execute_BIT_3_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_C);
   procedure Execute_BIT_3_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_D);
   procedure Execute_BIT_3_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_E);
   procedure Execute_BIT_3_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_H);
   procedure Execute_BIT_3_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_L);
   procedure Execute_BIT_3_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_A);
   procedure Execute_BIT_4_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_B);
   procedure Execute_BIT_4_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_C);
   procedure Execute_BIT_4_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_D);
   procedure Execute_BIT_4_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_E);
   procedure Execute_BIT_4_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_H);
   procedure Execute_BIT_4_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_L);
   procedure Execute_BIT_4_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_Addr_HL);
   procedure Execute_BIT_4_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_A);
   procedure Execute_BIT_5_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_B);
   procedure Execute_BIT_5_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_C);
   procedure Execute_BIT_5_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_D);
   procedure Execute_BIT_5_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_E);
   procedure Execute_BIT_5_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_H);
   procedure Execute_BIT_5_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_L);
   procedure Execute_BIT_5_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_Addr_HL);
   procedure Execute_BIT_5_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_A);
   procedure Execute_BIT_6_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_B);
   procedure Execute_BIT_6_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_C);
   procedure Execute_BIT_6_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_D);
   procedure Execute_BIT_6_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_E);
   procedure Execute_BIT_6_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_H);
   procedure Execute_BIT_6_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_L);
   procedure Execute_BIT_6_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_Addr_HL);
   procedure Execute_BIT_6_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_A);
   procedure Execute_BIT_7_B is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_B);
   procedure Execute_BIT_7_C is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_C);
   procedure Execute_BIT_7_D is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_D);
   procedure Execute_BIT_7_E is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_E);
   procedure Execute_BIT_7_H is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_H);
   procedure Execute_BIT_7_L is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_L);
   procedure Execute_BIT_7_Addr_HL is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_Addr_HL);
   procedure Execute_BIT_7_A is new Instructions.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_A);

end Gade.Dev.CPU.Instructions.Bitwise;
