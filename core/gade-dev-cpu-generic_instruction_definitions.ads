with Gade.Dev.CPU.Generic_Handlers;
with Gade.GB;

package Gade.Dev.CPU.Generic_Instruction_Definitions is
   package Handlers renames Gade.Dev.CPU.Generic_Handlers;

   procedure Execute_ADD_A_B is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADD,
      Source    => Handlers.SRC_B);

   procedure Execute_ADD_A_Addr_HL is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADD,
      Source    => Handlers.SRC_Addr_HL);

   procedure Execute_ADC_A_C is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADC,
      Source    => Handlers.SRC_C);

   procedure Execute_SUB_A_D is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SUB,
      Source    => Handlers.SRC_D);

   procedure Execute_SBC_A_E is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SBC,
      Source    => Handlers.SRC_E);

   procedure Execute_AND_A_H is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_AND,
      Source    => Handlers.SRC_H);

   procedure Execute_XOR_A_L is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_XOR,
      Source    => Handlers.SRC_L);

   procedure Execute_OR_A_A is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_OR,
      Source    => Handlers.SRC_A);

   procedure Execute_CP_A_Addr_HL is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_CP,
      Source    => Handlers.SRC_Addr_HL);

   procedure Execute_BIT_0_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test,
      Index     => 0,
      Target    => Handlers.SRC_B);

   procedure Execute_BIT_3_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test,
      Index     => 3,
      Target    => Handlers.SRC_Addr_HL);

   procedure Execute_RES_0_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_B);
   procedure Execute_RES_0_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_C);
   procedure Execute_RES_0_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_D);
   procedure Execute_RES_0_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_E);
   procedure Execute_RES_0_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_H);
   procedure Execute_RES_0_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_L);
   procedure Execute_RES_0_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_0_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 0, Target => Handlers.SRC_A);
   procedure Execute_RES_1_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_B);
   procedure Execute_RES_1_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_C);
   procedure Execute_RES_1_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_D);
   procedure Execute_RES_1_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_E);
   procedure Execute_RES_1_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_H);
   procedure Execute_RES_1_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_L);
   procedure Execute_RES_1_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_1_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 1, Target => Handlers.SRC_A);
   procedure Execute_RES_2_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_B);
   procedure Execute_RES_2_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_C);
   procedure Execute_RES_2_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_D);
   procedure Execute_RES_2_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_E);
   procedure Execute_RES_2_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_H);
   procedure Execute_RES_2_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_L);
   procedure Execute_RES_2_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_2_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 2, Target => Handlers.SRC_A);
   procedure Execute_RES_3_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_B);
   procedure Execute_RES_3_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_C);
   procedure Execute_RES_3_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_D);
   procedure Execute_RES_3_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_E);
   procedure Execute_RES_3_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_H);
   procedure Execute_RES_3_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_L);
   procedure Execute_RES_3_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_3_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 3, Target => Handlers.SRC_A);
   procedure Execute_RES_4_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_B);
   procedure Execute_RES_4_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_C);
   procedure Execute_RES_4_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_D);
   procedure Execute_RES_4_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_E);
   procedure Execute_RES_4_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_H);
   procedure Execute_RES_4_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_L);
   procedure Execute_RES_4_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_4_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 4, Target => Handlers.SRC_A);
   procedure Execute_RES_5_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_B);
   procedure Execute_RES_5_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_C);
   procedure Execute_RES_5_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_D);
   procedure Execute_RES_5_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_E);
   procedure Execute_RES_5_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_H);
   procedure Execute_RES_5_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_L);
   procedure Execute_RES_5_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_5_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 5, Target => Handlers.SRC_A);
   procedure Execute_RES_6_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_B);
   procedure Execute_RES_6_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_C);
   procedure Execute_RES_6_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_D);
   procedure Execute_RES_6_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_E);
   procedure Execute_RES_6_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_H);
   procedure Execute_RES_6_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_L);
   procedure Execute_RES_6_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_6_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 6, Target => Handlers.SRC_A);
   procedure Execute_RES_7_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_B);
   procedure Execute_RES_7_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_C);
   procedure Execute_RES_7_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_D);
   procedure Execute_RES_7_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_E);
   procedure Execute_RES_7_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_H);
   procedure Execute_RES_7_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_L);
   procedure Execute_RES_7_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_Addr_HL);
   procedure Execute_RES_7_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Reset, Index => 7, Target => Handlers.SRC_A);
   procedure Execute_SET_0_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_B);
   procedure Execute_SET_0_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_C);
   procedure Execute_SET_0_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_D);
   procedure Execute_SET_0_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_E);
   procedure Execute_SET_0_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_H);
   procedure Execute_SET_0_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_L);
   procedure Execute_SET_0_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_0_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 0, Target => Handlers.SRC_A);
   procedure Execute_SET_1_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_B);
   procedure Execute_SET_1_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_C);
   procedure Execute_SET_1_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_D);
   procedure Execute_SET_1_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_E);
   procedure Execute_SET_1_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_H);
   procedure Execute_SET_1_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_L);
   procedure Execute_SET_1_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_1_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 1, Target => Handlers.SRC_A);
   procedure Execute_SET_2_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_B);
   procedure Execute_SET_2_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_C);
   procedure Execute_SET_2_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_D);
   procedure Execute_SET_2_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_E);
   procedure Execute_SET_2_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_H);
   procedure Execute_SET_2_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_L);
   procedure Execute_SET_2_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_2_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 2, Target => Handlers.SRC_A);
   procedure Execute_SET_3_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_B);
   procedure Execute_SET_3_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_C);
   procedure Execute_SET_3_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_D);
   procedure Execute_SET_3_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_E);
   procedure Execute_SET_3_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_H);
   procedure Execute_SET_3_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_L);
   procedure Execute_SET_3_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_3_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 3, Target => Handlers.SRC_A);
   procedure Execute_SET_4_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_B);
   procedure Execute_SET_4_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_C);
   procedure Execute_SET_4_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_D);
   procedure Execute_SET_4_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_E);
   procedure Execute_SET_4_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_H);
   procedure Execute_SET_4_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_L);
   procedure Execute_SET_4_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_4_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 4, Target => Handlers.SRC_A);
   procedure Execute_SET_5_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_B);
   procedure Execute_SET_5_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_C);
   procedure Execute_SET_5_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_D);
   procedure Execute_SET_5_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_E);
   procedure Execute_SET_5_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_H);
   procedure Execute_SET_5_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_L);
   procedure Execute_SET_5_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_5_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 5, Target => Handlers.SRC_A);
   procedure Execute_SET_6_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_B);
   procedure Execute_SET_6_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_C);
   procedure Execute_SET_6_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_D);
   procedure Execute_SET_6_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_E);
   procedure Execute_SET_6_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_H);
   procedure Execute_SET_6_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_L);
   procedure Execute_SET_6_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_6_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 6, Target => Handlers.SRC_A);
   procedure Execute_SET_7_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_B);
   procedure Execute_SET_7_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_C);
   procedure Execute_SET_7_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_D);
   procedure Execute_SET_7_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_E);
   procedure Execute_SET_7_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_H);
   procedure Execute_SET_7_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_L);
   procedure Execute_SET_7_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_Addr_HL);
   procedure Execute_SET_7_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Set, Index => 7, Target => Handlers.SRC_A);


   procedure Execute_LD_BC_Imm16 is new Handlers.Execute_LD_Word
     (Dest   => Handlers.REG_BC,
      Source => Handlers.WSRC_Imm16);

   procedure Execute_LD_DE_Imm16 is new Handlers.Execute_LD_Word
     (Dest   => Handlers.REG_DE,
      Source => Handlers.WSRC_Imm16);

   procedure Execute_LD_HL_Imm16 is new Handlers.Execute_LD_Word
     (Dest   => Handlers.REG_HL,
      Source => Handlers.WSRC_Imm16);

   procedure Execute_LD_SP_Imm16 is new Handlers.Execute_LD_Word
     (Dest   => Handlers.REG_SP,
      Source => Handlers.WSRC_Imm16);

   procedure Execute_LD_Addr_Imm16_SP
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_LD_Addr_Imm16_SP;

   procedure Execute_LD_Addr_BC_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_BC,
      Source => Handlers.SRC_A);

   procedure Execute_LD_A_Addr_BC is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_Addr_BC);

   procedure Execute_LD_Addr_DE_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_DE,
      Source => Handlers.SRC_A);

   procedure Execute_LD_A_Addr_DE is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_Addr_DE);

   procedure Execute_LD_Addr_HL_Inc_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_HL_Inc,
      Source => Handlers.SRC_A);

   procedure Execute_LD_A_Addr_HL_Inc is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_Addr_HL_Inc);

   procedure Execute_LD_Addr_HL_Dec_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_HL_Dec,
      Source => Handlers.SRC_A);

   procedure Execute_LD_A_Addr_HL_Dec is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_Addr_HL_Dec);

   procedure Execute_LD_B_Imm8 is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_B,
      Source => Handlers.SRC_Imm8);

   procedure Execute_LD_C_Imm8 is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_C,
      Source => Handlers.SRC_Imm8);

   procedure Execute_LD_D_Imm8 is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_D,
      Source => Handlers.SRC_Imm8);

   procedure Execute_LD_E_Imm8 is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_E,
      Source => Handlers.SRC_Imm8);

   procedure Execute_LD_H_Imm8 is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_H,
      Source => Handlers.SRC_Imm8);

   procedure Execute_LD_L_Imm8 is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_L,
      Source => Handlers.SRC_Imm8);

   procedure Execute_LD_B_C is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_B,
      Source => Handlers.SRC_C);

   procedure Execute_LD_B_B is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_B,
      Source => Handlers.SRC_B);

   procedure Execute_LD_B_D is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_B,
      Source => Handlers.SRC_D);

   procedure Execute_LD_B_E is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_B,
      Source => Handlers.SRC_E);

   procedure Execute_LD_B_H is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_B,
      Source => Handlers.SRC_H);

   procedure Execute_LD_B_L is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_B,
      Source => Handlers.SRC_L);

   procedure Execute_LD_B_Addr_HL is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_B,
      Source => Handlers.SRC_Addr_HL);

   procedure Execute_LD_B_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_B,
      Source => Handlers.SRC_A);

   procedure Execute_LD_C_B is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_C,
      Source => Handlers.SRC_B);

   procedure Execute_LD_C_C is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_C,
      Source => Handlers.SRC_C);

   procedure Execute_LD_C_D is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_C,
      Source => Handlers.SRC_D);

   procedure Execute_LD_C_E is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_C,
      Source => Handlers.SRC_E);

   procedure Execute_LD_C_H is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_C,
      Source => Handlers.SRC_H);

   procedure Execute_LD_C_L is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_C,
      Source => Handlers.SRC_L);

   procedure Execute_LD_C_Addr_HL is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_C,
      Source => Handlers.SRC_Addr_HL);

   procedure Execute_LD_C_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_C,
      Source => Handlers.SRC_A);

   procedure Execute_LD_D_B is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_D,
      Source => Handlers.SRC_B);

   procedure Execute_LD_D_C is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_D,
      Source => Handlers.SRC_C);

   procedure Execute_LD_D_D is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_D,
      Source => Handlers.SRC_D);

   procedure Execute_LD_D_E is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_D,
      Source => Handlers.SRC_E);

   procedure Execute_LD_D_H is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_D,
      Source => Handlers.SRC_H);

   procedure Execute_LD_D_L is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_D,
      Source => Handlers.SRC_L);

   procedure Execute_LD_D_Addr_HL is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_D,
      Source => Handlers.SRC_Addr_HL);

   procedure Execute_LD_D_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_D,
      Source => Handlers.SRC_A);

   procedure Execute_LD_E_B is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_E,
      Source => Handlers.SRC_B);

   procedure Execute_LD_E_C is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_E,
      Source => Handlers.SRC_C);

   procedure Execute_LD_E_D is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_E,
      Source => Handlers.SRC_D);

   procedure Execute_LD_E_E is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_E,
      Source => Handlers.SRC_E);

   procedure Execute_LD_E_H is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_E,
      Source => Handlers.SRC_H);

   procedure Execute_LD_E_L is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_E,
      Source => Handlers.SRC_L);

   procedure Execute_LD_E_Addr_HL is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_E,
      Source => Handlers.SRC_Addr_HL);

   procedure Execute_LD_E_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_E,
      Source => Handlers.SRC_A);

   procedure Execute_LD_H_B is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_H,
      Source => Handlers.SRC_B);

   procedure Execute_LD_H_C is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_H,
      Source => Handlers.SRC_C);

   procedure Execute_LD_H_D is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_H,
      Source => Handlers.SRC_D);

   procedure Execute_LD_H_E is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_H,
      Source => Handlers.SRC_E);

   procedure Execute_LD_H_H is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_H,
      Source => Handlers.SRC_H);

   procedure Execute_LD_H_L is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_H,
      Source => Handlers.SRC_L);

   procedure Execute_LD_H_Addr_HL is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_H,
      Source => Handlers.SRC_Addr_HL);

   procedure Execute_LD_H_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_H,
      Source => Handlers.SRC_A);

   procedure Execute_LD_L_B is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_L,
      Source => Handlers.SRC_B);

   procedure Execute_LD_L_C is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_L,
      Source => Handlers.SRC_C);

   procedure Execute_LD_L_D is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_L,
      Source => Handlers.SRC_D);

   procedure Execute_LD_L_E is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_L,
      Source => Handlers.SRC_E);

   procedure Execute_LD_L_H is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_L,
      Source => Handlers.SRC_H);

   procedure Execute_LD_L_L is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_L,
      Source => Handlers.SRC_L);

   procedure Execute_LD_L_Addr_HL is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_L,
      Source => Handlers.SRC_Addr_HL);

   procedure Execute_LD_L_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_L,
      Source => Handlers.SRC_A);

   procedure Execute_LD_Addr_HL_B is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_HL,
      Source => Handlers.SRC_B);

   procedure Execute_LD_Addr_HL_C is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_HL,
      Source => Handlers.SRC_C);

   procedure Execute_LD_Addr_HL_D is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_HL,
      Source => Handlers.SRC_D);

   procedure Execute_LD_Addr_HL_E is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_HL,
      Source => Handlers.SRC_E);

   procedure Execute_LD_Addr_HL_H is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_HL,
      Source => Handlers.SRC_H);

   procedure Execute_LD_Addr_HL_L is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_HL,
      Source => Handlers.SRC_L);

   procedure Execute_LD_Addr_HL_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_HL,
      Source => Handlers.SRC_A);

   procedure Execute_LD_A_B is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_B);

   procedure Execute_LD_A_C is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_C);

   procedure Execute_LD_A_D is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_D);

   procedure Execute_LD_A_E is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_E);

   procedure Execute_LD_A_H is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_H);

   procedure Execute_LD_A_L is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_L);

   procedure Execute_LD_A_Addr_HL is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_Addr_HL);

   procedure Execute_LD_A_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_A);

   procedure Execute_LD_A_Imm8 is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_Imm8);

   procedure Execute_LD_Addr_HL_Imm8 is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_HL,
      Source => Handlers.SRC_Imm8);

   procedure Execute_LD_High_Addr_Imm8_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_High_Addr_Imm8,
      Source => Handlers.SRC_A);

   procedure Execute_LD_High_Addr_C_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_High_Addr_C,
      Source => Handlers.SRC_A);

   procedure Execute_LD_Addr_Imm16_A is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_Addr_Imm16,
      Source => Handlers.SRC_A);

   procedure Execute_LD_A_High_Addr_Imm8 is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_High_Addr_Imm8);

   procedure Execute_LD_A_High_Addr_C is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_High_Addr_C);

   procedure Execute_LD_SP_HL is new Handlers.Execute_LD_Word
     (Dest   => Handlers.REG_SP,
      Source => Handlers.WSRC_HL);

   procedure Execute_LD_A_Addr_Imm16 is new Handlers.Execute_LD_Byte
     (Dest   => Handlers.DST_A,
      Source => Handlers.SRC_Addr_Imm16);

   procedure Execute_LD_HL_SP_Plus_Imm8
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_LD_HL_SP_Plus_Imm8;

   procedure Execute_INC_BC is new Handlers.Execute_Inc_Dec_Word
     (Operation => Handlers.OP_INC,
      Target    => Handlers.REG_BC);

   procedure Execute_INC_B is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_INC,
      Target    => Handlers.DST_B);

   procedure Execute_DEC_B is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.DST_B);

   procedure Execute_DEC_BC is new Handlers.Execute_Inc_Dec_Word
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.REG_BC);

   procedure Execute_INC_C is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_INC,
      Target    => Handlers.DST_C);

   procedure Execute_DEC_C is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.DST_C);

   procedure Execute_INC_DE is new Handlers.Execute_Inc_Dec_Word
     (Operation => Handlers.OP_INC,
      Target    => Handlers.REG_DE);

   procedure Execute_INC_D is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_INC,
      Target    => Handlers.DST_D);

   procedure Execute_DEC_D is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.DST_D);

   procedure Execute_DEC_DE is new Handlers.Execute_Inc_Dec_Word
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.REG_DE);

   procedure Execute_INC_E is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_INC,
      Target    => Handlers.DST_E);

   procedure Execute_DEC_E is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.DST_E);

   procedure Execute_INC_H is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_INC,
      Target    => Handlers.DST_H);

   procedure Execute_DEC_H is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.DST_H);

   procedure Execute_INC_L is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_INC,
      Target    => Handlers.DST_L);

   procedure Execute_DEC_L is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.DST_L);

   procedure Execute_INC_SP is new Handlers.Execute_Inc_Dec_Word
     (Operation => Handlers.OP_INC,
      Target    => Handlers.REG_SP);

   procedure Execute_INC_Addr_HL is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_INC,
      Target    => Handlers.DST_Addr_HL);

   procedure Execute_DEC_Addr_HL is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.DST_Addr_HL);

   procedure Execute_INC_HL is new Handlers.Execute_Inc_Dec_Word
     (Operation => Handlers.OP_INC,
      Target    => Handlers.REG_HL);

   procedure Execute_DEC_HL is new Handlers.Execute_Inc_Dec_Word
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.REG_HL);

   procedure Execute_DEC_SP is new Handlers.Execute_Inc_Dec_Word
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.REG_SP);

   procedure Execute_INC_A is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_INC,
      Target    => Handlers.DST_A);

   procedure Execute_DEC_A is new Handlers.Execute_Inc_Dec_Byte
     (Operation => Handlers.OP_DEC,
      Target    => Handlers.DST_A);

   procedure Execute_RLCA is new Handlers.Execute_Rotate_Shift
     (Operation    => Handlers.ROT_RLC,
      Target       => Handlers.DST_A,
      Adjust_Flags => False);

   procedure Execute_RLA is new Handlers.Execute_Rotate_Shift
     (Operation    => Handlers.ROT_RL,
      Target       => Handlers.DST_A,
      Adjust_Flags => False);

   procedure Execute_RRCA is new Handlers.Execute_Rotate_Shift
     (Operation    => Handlers.ROT_RRC,
      Target       => Handlers.DST_A,
      Adjust_Flags => False);

   procedure Execute_RRA is new Handlers.Execute_Rotate_Shift
     (Operation    => Handlers.ROT_RR,
      Target       => Handlers.DST_A,
      Adjust_Flags => False);

   procedure Execute_RLC_B is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RLC,
      Target    => Handlers.DST_B);

   procedure Execute_RLC_C is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RLC,
      Target    => Handlers.DST_C);

   procedure Execute_RLC_D is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RLC,
      Target    => Handlers.DST_D);

   procedure Execute_RLC_E is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RLC,
      Target    => Handlers.DST_E);

   procedure Execute_RLC_H is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RLC,
      Target    => Handlers.DST_H);

   procedure Execute_RLC_L is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RLC,
      Target    => Handlers.DST_L);

   procedure Execute_RLC_Addr_HL is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RLC,
      Target    => Handlers.DST_Addr_HL);

   procedure Execute_RLC_A is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RLC,
      Target    => Handlers.DST_A);

   procedure Execute_RRC_B is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RRC,
      Target    => Handlers.DST_B);

   procedure Execute_RRC_C is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RRC,
      Target    => Handlers.DST_C);

   procedure Execute_RRC_D is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RRC,
      Target    => Handlers.DST_D);

   procedure Execute_RRC_E is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RRC,
      Target    => Handlers.DST_E);

   procedure Execute_RRC_H is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RRC,
      Target    => Handlers.DST_H);

   procedure Execute_RRC_L is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RRC,
      Target    => Handlers.DST_L);

   procedure Execute_RL_C is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RL,
      Target    => Handlers.DST_C);

   procedure Execute_RRC_A is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RRC,
      Target    => Handlers.DST_A);

   procedure Execute_RRC_Addr_HL is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RRC,
      Target    => Handlers.DST_Addr_HL);

   procedure Execute_RL_B is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RL,
      Target    => Handlers.DST_B);

   procedure Execute_RL_D is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RL,
      Target    => Handlers.DST_D);

   procedure Execute_RL_E is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RL,
      Target    => Handlers.DST_E);

   procedure Execute_RL_H is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RL,
      Target    => Handlers.DST_H);

   procedure Execute_RL_L is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RL,
      Target    => Handlers.DST_L);

   procedure Execute_RL_Addr_HL is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RL,
      Target    => Handlers.DST_Addr_HL);

   procedure Execute_RL_A is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RL,
      Target    => Handlers.DST_A);

   procedure Execute_RR_B is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RR,
      Target    => Handlers.DST_B);

   procedure Execute_RR_C is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RR,
      Target    => Handlers.DST_C);

   procedure Execute_RR_D is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RR,
      Target    => Handlers.DST_D);

   procedure Execute_RR_E is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RR,
      Target    => Handlers.DST_E);

   procedure Execute_RR_H is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RR,
      Target    => Handlers.DST_H);

   procedure Execute_RR_L is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RR,
      Target    => Handlers.DST_L);

   procedure Execute_RR_Addr_HL is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RR,
      Target    => Handlers.DST_Addr_HL);

   procedure Execute_RR_A is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_RR,
      Target    => Handlers.DST_A);

   procedure Execute_SLA_B is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SLA,
      Target    => Handlers.DST_B);

   procedure Execute_SLA_C is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SLA,
      Target    => Handlers.DST_C);

   procedure Execute_SLA_D is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SLA,
      Target    => Handlers.DST_D);

   procedure Execute_SLA_E is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SLA,
      Target    => Handlers.DST_E);

   procedure Execute_SLA_H is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SLA,
      Target    => Handlers.DST_H);

   procedure Execute_SLA_L is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SLA,
      Target    => Handlers.DST_L);

   procedure Execute_SLA_Addr_HL is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SLA,
      Target    => Handlers.DST_Addr_HL);

   procedure Execute_SLA_A is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SLA,
      Target    => Handlers.DST_A);

   procedure Execute_SRA_B is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRA,
      Target    => Handlers.DST_B);

   procedure Execute_SRA_C is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRA,
      Target    => Handlers.DST_C);

   procedure Execute_SRA_D is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRA,
      Target    => Handlers.DST_D);

   procedure Execute_SRA_E is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRA,
      Target    => Handlers.DST_E);

   procedure Execute_SRA_H is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRA,
      Target    => Handlers.DST_H);

   procedure Execute_SRA_L is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRA,
      Target    => Handlers.DST_L);

   procedure Execute_SRA_Addr_HL is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRA,
      Target    => Handlers.DST_Addr_HL);

   procedure Execute_SRA_A is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRA,
      Target    => Handlers.DST_A);

   procedure Execute_SWAP_B is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SWAP,
      Target    => Handlers.DST_B);

   procedure Execute_SWAP_C is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SWAP,
      Target    => Handlers.DST_C);

   procedure Execute_SWAP_D is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SWAP,
      Target    => Handlers.DST_D);

   procedure Execute_SWAP_E is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SWAP,
      Target    => Handlers.DST_E);

   procedure Execute_SWAP_H is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SWAP,
      Target    => Handlers.DST_H);

   procedure Execute_SWAP_L is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SWAP,
      Target    => Handlers.DST_L);

   procedure Execute_SWAP_Addr_HL is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SWAP,
      Target    => Handlers.DST_Addr_HL);

   procedure Execute_SWAP_A is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SWAP,
      Target    => Handlers.DST_A);

   procedure Execute_SRL_B is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRL,
      Target    => Handlers.DST_B);

   procedure Execute_SRL_C is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRL,
      Target    => Handlers.DST_C);

   procedure Execute_SRL_D is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRL,
      Target    => Handlers.DST_D);

   procedure Execute_SRL_E is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRL,
      Target    => Handlers.DST_E);

   procedure Execute_SRL_H is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRL,
      Target    => Handlers.DST_H);

   procedure Execute_SRL_L is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRL,
      Target    => Handlers.DST_L);

   procedure Execute_SRL_Addr_HL is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRL,
      Target    => Handlers.DST_Addr_HL);

   procedure Execute_SRL_A is new Handlers.Execute_Rotate_Shift
     (Operation => Handlers.ROT_SRL,
      Target    => Handlers.DST_A);

   procedure Execute_PUSH_BC is new Handlers.Execute_Push
     (Source => Handlers.REG_BC);

   procedure Execute_PUSH_DE is new Handlers.Execute_Push
     (Source => Handlers.REG_DE);

   procedure Execute_PUSH_HL is new Handlers.Execute_Push
     (Source => Handlers.REG_HL);

   procedure Execute_PUSH_AF is new Handlers.Execute_Push
     (Source => Handlers.REG_AF);

   procedure Execute_POP_BC is new Handlers.Execute_Pop
     (Dest => Handlers.REG_BC);

   procedure Execute_POP_DE is new Handlers.Execute_Pop
     (Dest => Handlers.REG_DE);

   procedure Execute_POP_HL is new Handlers.Execute_Pop
     (Dest => Handlers.REG_HL);

   procedure Execute_POP_AF is new Handlers.Execute_Pop
     (Dest => Handlers.REG_AF);

   procedure Execute_JR is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_JR);

   procedure Execute_JR_NZ is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_JR,
      Condition => Handlers.JCOND_NZ);

   procedure Execute_JR_Z is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_JR,
      Condition => Handlers.JCOND_Z);

   procedure Execute_JR_NC is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_JR,
      Condition => Handlers.JCOND_NC);

   procedure Execute_JR_C is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_JR,
      Condition => Handlers.JCOND_C);

   procedure Execute_JP is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_JP);

   procedure Execute_JP_NZ is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_JP,
      Condition => Handlers.JCOND_NZ);

   procedure Execute_JP_Z is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_JP,
      Condition => Handlers.JCOND_Z);

   procedure Execute_JP_NC is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_JP,
      Condition => Handlers.JCOND_NC);

   procedure Execute_JP_C is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_JP,
      Condition => Handlers.JCOND_C);

   procedure Execute_JP_HL is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_JP,
      Target    => Handlers.JTARGET_HL);

   procedure Execute_CALL is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_CALL);

   procedure Execute_CALL_NZ is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_CALL,
      Condition => Handlers.JCOND_NZ);

   procedure Execute_CALL_Z is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_CALL,
      Condition => Handlers.JCOND_Z);

   procedure Execute_CALL_NC is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_CALL,
      Condition => Handlers.JCOND_NC);

   procedure Execute_CALL_C is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_CALL,
      Condition => Handlers.JCOND_C);

   procedure Execute_RET_NZ is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RET,
      Condition => Handlers.JCOND_NZ);

   procedure Execute_RET_Z is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RET,
      Condition => Handlers.JCOND_Z);

   procedure Execute_RET is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RET);

   procedure Execute_RET_NC is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RET,
      Condition => Handlers.JCOND_NC);

   procedure Execute_RET_C is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RET,
      Condition => Handlers.JCOND_C);

   procedure Execute_RETI is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RETI);

   procedure Execute_RST_00 is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RST,
      Vector    => 16#0000#);

   procedure Execute_RST_08 is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RST,
      Vector    => 16#0008#);

   procedure Execute_RST_10 is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RST,
      Vector    => 16#0010#);

   procedure Execute_RST_18 is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RST,
      Vector    => 16#0018#);

   procedure Execute_RST_20 is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RST,
      Vector    => 16#0020#);

   procedure Execute_RST_28 is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RST,
      Vector    => 16#0028#);

   procedure Execute_RST_30 is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RST,
      Vector    => 16#0030#);

   procedure Execute_RST_38 is new Handlers.Execute_Flow
     (Operation => Handlers.FLOW_RST,
      Vector    => 16#0038#);

   procedure Execute_NOP
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_NOP;

   procedure Execute_ADD_HL_BC is new Handlers.Execute_Add_HL
     (Source => Handlers.REG_BC);

   procedure Execute_ADD_HL_DE is new Handlers.Execute_Add_HL
     (Source => Handlers.REG_DE);

   procedure Execute_ADD_HL_HL is new Handlers.Execute_Add_HL
     (Source => Handlers.REG_HL);

   procedure Execute_ADD_HL_SP is new Handlers.Execute_Add_HL
     (Source => Handlers.REG_SP);

   procedure Execute_ADD_SP_Imm8
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_Add_SP_Imm8;

   procedure Execute_DAA
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_DAA;

   procedure Execute_CPL
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_CPL;

   procedure Execute_SCF
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_SCF;

   procedure Execute_CCF
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_CCF;

   procedure Execute_HALT
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_HALT;

   procedure Execute_STOP
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_STOP;

   procedure Execute_DI
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_DI;

   procedure Execute_EI
     (GB : in out Gade.GB.GB_Type) renames Handlers.Execute_EI;

   procedure Execute_ADD_A_C is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADD,
      Source    => Handlers.SRC_C);

   procedure Execute_ADD_A_D is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADD,
      Source    => Handlers.SRC_D);

   procedure Execute_ADD_A_E is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADD,
      Source    => Handlers.SRC_E);

   procedure Execute_ADD_A_H is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADD,
      Source    => Handlers.SRC_H);

   procedure Execute_ADD_A_L is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADD,
      Source    => Handlers.SRC_L);

   procedure Execute_ADD_A_A is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADD,
      Source    => Handlers.SRC_A);

   procedure Execute_ADC_A_B is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADC,
      Source    => Handlers.SRC_B);

   procedure Execute_ADC_A_D is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADC,
      Source    => Handlers.SRC_D);

   procedure Execute_ADC_A_E is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADC,
      Source    => Handlers.SRC_E);

   procedure Execute_ADC_A_H is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADC,
      Source    => Handlers.SRC_H);

   procedure Execute_ADC_A_L is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADC,
      Source    => Handlers.SRC_L);

   procedure Execute_ADC_A_Addr_HL is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADC,
      Source    => Handlers.SRC_Addr_HL);

   procedure Execute_ADC_A_A is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADC,
      Source    => Handlers.SRC_A);

   procedure Execute_SUB_A_B is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SUB,
      Source    => Handlers.SRC_B);

   procedure Execute_SUB_A_C is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SUB,
      Source    => Handlers.SRC_C);

   procedure Execute_SUB_A_E is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SUB,
      Source    => Handlers.SRC_E);

   procedure Execute_SUB_A_H is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SUB,
      Source    => Handlers.SRC_H);

   procedure Execute_SUB_A_L is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SUB,
      Source    => Handlers.SRC_L);

   procedure Execute_SUB_A_Addr_HL is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SUB,
      Source    => Handlers.SRC_Addr_HL);

   procedure Execute_SUB_A_A is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SUB,
      Source    => Handlers.SRC_A);

   procedure Execute_SBC_A_B is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SBC,
      Source    => Handlers.SRC_B);

   procedure Execute_SBC_A_C is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SBC,
      Source    => Handlers.SRC_C);

   procedure Execute_SBC_A_D is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SBC,
      Source    => Handlers.SRC_D);

   procedure Execute_SBC_A_H is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SBC,
      Source    => Handlers.SRC_H);

   procedure Execute_SBC_A_L is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SBC,
      Source    => Handlers.SRC_L);

   procedure Execute_SBC_A_Addr_HL is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SBC,
      Source    => Handlers.SRC_Addr_HL);

   procedure Execute_SBC_A_A is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SBC,
      Source    => Handlers.SRC_A);

   procedure Execute_AND_A_B is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_AND,
      Source    => Handlers.SRC_B);

   procedure Execute_AND_A_C is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_AND,
      Source    => Handlers.SRC_C);

   procedure Execute_AND_A_D is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_AND,
      Source    => Handlers.SRC_D);

   procedure Execute_AND_A_E is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_AND,
      Source    => Handlers.SRC_E);

   procedure Execute_AND_A_L is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_AND,
      Source    => Handlers.SRC_L);

   procedure Execute_AND_A_Addr_HL is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_AND,
      Source    => Handlers.SRC_Addr_HL);

   procedure Execute_AND_A_A is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_AND,
      Source    => Handlers.SRC_A);

   procedure Execute_XOR_A_B is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_XOR,
      Source    => Handlers.SRC_B);

   procedure Execute_XOR_A_C is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_XOR,
      Source    => Handlers.SRC_C);

   procedure Execute_XOR_A_D is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_XOR,
      Source    => Handlers.SRC_D);

   procedure Execute_XOR_A_E is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_XOR,
      Source    => Handlers.SRC_E);

   procedure Execute_XOR_A_H is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_XOR,
      Source    => Handlers.SRC_H);

   procedure Execute_XOR_A_Addr_HL is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_XOR,
      Source    => Handlers.SRC_Addr_HL);

   procedure Execute_XOR_A_A is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_XOR,
      Source    => Handlers.SRC_A);

   procedure Execute_OR_A_B is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_OR,
      Source    => Handlers.SRC_B);

   procedure Execute_OR_A_C is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_OR,
      Source    => Handlers.SRC_C);

   procedure Execute_OR_A_D is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_OR,
      Source    => Handlers.SRC_D);

   procedure Execute_OR_A_E is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_OR,
      Source    => Handlers.SRC_E);

   procedure Execute_OR_A_H is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_OR,
      Source    => Handlers.SRC_H);

   procedure Execute_OR_A_L is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_OR,
      Source    => Handlers.SRC_L);

   procedure Execute_OR_A_Addr_HL is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_OR,
      Source    => Handlers.SRC_Addr_HL);

   procedure Execute_CP_A_B is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_CP,
      Source    => Handlers.SRC_B);

   procedure Execute_CP_A_C is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_CP,
      Source    => Handlers.SRC_C);

   procedure Execute_CP_A_D is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_CP,
      Source    => Handlers.SRC_D);

   procedure Execute_CP_A_E is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_CP,
      Source    => Handlers.SRC_E);

   procedure Execute_CP_A_H is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_CP,
      Source    => Handlers.SRC_H);

   procedure Execute_CP_A_L is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_CP,
      Source    => Handlers.SRC_L);

   procedure Execute_CP_A_A is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_CP,
      Source    => Handlers.SRC_A);

   procedure Execute_ADD_A_Imm8 is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADD,
      Source    => Handlers.SRC_Imm8);

   procedure Execute_ADC_A_Imm8 is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_ADC,
      Source    => Handlers.SRC_Imm8);

   procedure Execute_SUB_A_Imm8 is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SUB,
      Source    => Handlers.SRC_Imm8);

   procedure Execute_SBC_A_Imm8 is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_SBC,
      Source    => Handlers.SRC_Imm8);

   procedure Execute_AND_A_Imm8 is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_AND,
      Source    => Handlers.SRC_Imm8);

   procedure Execute_XOR_A_Imm8 is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_XOR,
      Source    => Handlers.SRC_Imm8);

   procedure Execute_OR_A_Imm8 is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_OR,
      Source    => Handlers.SRC_Imm8);

   procedure Execute_CP_A_Imm8 is new Handlers.Execute_ALU_A_Source
     (Operation => Handlers.ALU_CP,
      Source    => Handlers.SRC_Imm8);

   procedure Execute_BIT_0_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 0, Target => Handlers.SRC_C);
   procedure Execute_BIT_0_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 0, Target => Handlers.SRC_D);
   procedure Execute_BIT_0_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 0, Target => Handlers.SRC_E);
   procedure Execute_BIT_0_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 0, Target => Handlers.SRC_H);
   procedure Execute_BIT_0_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 0, Target => Handlers.SRC_L);
   procedure Execute_BIT_0_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 0, Target => Handlers.SRC_Addr_HL);
   procedure Execute_BIT_0_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 0, Target => Handlers.SRC_A);
   procedure Execute_BIT_1_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 1, Target => Handlers.SRC_B);
   procedure Execute_BIT_1_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 1, Target => Handlers.SRC_C);
   procedure Execute_BIT_1_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 1, Target => Handlers.SRC_D);
   procedure Execute_BIT_1_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 1, Target => Handlers.SRC_E);
   procedure Execute_BIT_1_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 1, Target => Handlers.SRC_H);
   procedure Execute_BIT_1_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 1, Target => Handlers.SRC_L);
   procedure Execute_BIT_1_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 1, Target => Handlers.SRC_Addr_HL);
   procedure Execute_BIT_1_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 1, Target => Handlers.SRC_A);
   procedure Execute_BIT_2_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 2, Target => Handlers.SRC_B);
   procedure Execute_BIT_2_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 2, Target => Handlers.SRC_C);
   procedure Execute_BIT_2_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 2, Target => Handlers.SRC_D);
   procedure Execute_BIT_2_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 2, Target => Handlers.SRC_E);
   procedure Execute_BIT_2_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 2, Target => Handlers.SRC_H);
   procedure Execute_BIT_2_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 2, Target => Handlers.SRC_L);
   procedure Execute_BIT_2_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 2, Target => Handlers.SRC_Addr_HL);
   procedure Execute_BIT_2_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 2, Target => Handlers.SRC_A);
   procedure Execute_BIT_3_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 3, Target => Handlers.SRC_B);
   procedure Execute_BIT_3_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 3, Target => Handlers.SRC_C);
   procedure Execute_BIT_3_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 3, Target => Handlers.SRC_D);
   procedure Execute_BIT_3_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 3, Target => Handlers.SRC_E);
   procedure Execute_BIT_3_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 3, Target => Handlers.SRC_H);
   procedure Execute_BIT_3_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 3, Target => Handlers.SRC_L);
   procedure Execute_BIT_3_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 3, Target => Handlers.SRC_A);
   procedure Execute_BIT_4_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 4, Target => Handlers.SRC_B);
   procedure Execute_BIT_4_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 4, Target => Handlers.SRC_C);
   procedure Execute_BIT_4_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 4, Target => Handlers.SRC_D);
   procedure Execute_BIT_4_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 4, Target => Handlers.SRC_E);
   procedure Execute_BIT_4_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 4, Target => Handlers.SRC_H);
   procedure Execute_BIT_4_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 4, Target => Handlers.SRC_L);
   procedure Execute_BIT_4_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 4, Target => Handlers.SRC_Addr_HL);
   procedure Execute_BIT_4_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 4, Target => Handlers.SRC_A);
   procedure Execute_BIT_5_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 5, Target => Handlers.SRC_B);
   procedure Execute_BIT_5_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 5, Target => Handlers.SRC_C);
   procedure Execute_BIT_5_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 5, Target => Handlers.SRC_D);
   procedure Execute_BIT_5_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 5, Target => Handlers.SRC_E);
   procedure Execute_BIT_5_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 5, Target => Handlers.SRC_H);
   procedure Execute_BIT_5_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 5, Target => Handlers.SRC_L);
   procedure Execute_BIT_5_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 5, Target => Handlers.SRC_Addr_HL);
   procedure Execute_BIT_5_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 5, Target => Handlers.SRC_A);
   procedure Execute_BIT_6_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 6, Target => Handlers.SRC_B);
   procedure Execute_BIT_6_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 6, Target => Handlers.SRC_C);
   procedure Execute_BIT_6_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 6, Target => Handlers.SRC_D);
   procedure Execute_BIT_6_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 6, Target => Handlers.SRC_E);
   procedure Execute_BIT_6_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 6, Target => Handlers.SRC_H);
   procedure Execute_BIT_6_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 6, Target => Handlers.SRC_L);
   procedure Execute_BIT_6_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 6, Target => Handlers.SRC_Addr_HL);
   procedure Execute_BIT_6_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 6, Target => Handlers.SRC_A);
   procedure Execute_BIT_7_B is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 7, Target => Handlers.SRC_B);
   procedure Execute_BIT_7_C is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 7, Target => Handlers.SRC_C);
   procedure Execute_BIT_7_D is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 7, Target => Handlers.SRC_D);
   procedure Execute_BIT_7_E is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 7, Target => Handlers.SRC_E);
   procedure Execute_BIT_7_H is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 7, Target => Handlers.SRC_H);
   procedure Execute_BIT_7_L is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 7, Target => Handlers.SRC_L);
   procedure Execute_BIT_7_Addr_HL is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 7, Target => Handlers.SRC_Addr_HL);
   procedure Execute_BIT_7_A is new Handlers.Execute_Bit_Source
     (Operation => Handlers.BIT_Test, Index => 7, Target => Handlers.SRC_A);

end Gade.Dev.CPU.Generic_Instruction_Definitions;
