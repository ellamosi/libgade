package Gade.Dev.CPU.Instructions.Bitwise.Instances is

   --  Accumulator rotates
   procedure RLCA is new Rotate_Shift (ROT_RLC, DST_A, False);
   procedure RLA is new Rotate_Shift (ROT_RL, DST_A, False);

   procedure RRCA is new Rotate_Shift (ROT_RRC, DST_A, False);
   procedure RRA is new Rotate_Shift (ROT_RR, DST_A, False);

   --  RLC
   procedure RLC_A is new Rotate_Shift (ROT_RLC, DST_A);
   procedure RLC_B is new Rotate_Shift (ROT_RLC, DST_B);
   procedure RLC_C is new Rotate_Shift (ROT_RLC, DST_C);
   procedure RLC_D is new Rotate_Shift (ROT_RLC, DST_D);
   procedure RLC_E is new Rotate_Shift (ROT_RLC, DST_E);
   procedure RLC_H is new Rotate_Shift (ROT_RLC, DST_H);
   procedure RLC_L is new Rotate_Shift (ROT_RLC, DST_L);
   procedure RLC_Addr_HL is new Rotate_Shift (ROT_RLC, DST_Addr_HL);

   --  RRC
   procedure RRC_A is new Rotate_Shift (ROT_RRC, DST_A);
   procedure RRC_B is new Rotate_Shift (ROT_RRC, DST_B);
   procedure RRC_C is new Rotate_Shift (ROT_RRC, DST_C);
   procedure RRC_D is new Rotate_Shift (ROT_RRC, DST_D);
   procedure RRC_E is new Rotate_Shift (ROT_RRC, DST_E);
   procedure RRC_H is new Rotate_Shift (ROT_RRC, DST_H);
   procedure RRC_L is new Rotate_Shift (ROT_RRC, DST_L);
   procedure RRC_Addr_HL is new Rotate_Shift (ROT_RRC, DST_Addr_HL);

   --  RL
   procedure RL_A is new Rotate_Shift (ROT_RL, DST_A);
   procedure RL_B is new Rotate_Shift (ROT_RL, DST_B);
   procedure RL_C is new Rotate_Shift (ROT_RL, DST_C);
   procedure RL_D is new Rotate_Shift (ROT_RL, DST_D);
   procedure RL_E is new Rotate_Shift (ROT_RL, DST_E);
   procedure RL_H is new Rotate_Shift (ROT_RL, DST_H);
   procedure RL_L is new Rotate_Shift (ROT_RL, DST_L);
   procedure RL_Addr_HL is new Rotate_Shift (ROT_RL, DST_Addr_HL);

   --  RR
   procedure RR_A is new Rotate_Shift (ROT_RR, DST_A);
   procedure RR_B is new Rotate_Shift (ROT_RR, DST_B);
   procedure RR_C is new Rotate_Shift (ROT_RR, DST_C);
   procedure RR_D is new Rotate_Shift (ROT_RR, DST_D);
   procedure RR_E is new Rotate_Shift (ROT_RR, DST_E);
   procedure RR_H is new Rotate_Shift (ROT_RR, DST_H);
   procedure RR_L is new Rotate_Shift (ROT_RR, DST_L);
   procedure RR_Addr_HL is new Rotate_Shift (ROT_RR, DST_Addr_HL);

   --  SLA
   procedure SLA_A is new Rotate_Shift (ROT_SLA, DST_A);
   procedure SLA_B is new Rotate_Shift (ROT_SLA, DST_B);
   procedure SLA_C is new Rotate_Shift (ROT_SLA, DST_C);
   procedure SLA_D is new Rotate_Shift (ROT_SLA, DST_D);
   procedure SLA_E is new Rotate_Shift (ROT_SLA, DST_E);
   procedure SLA_H is new Rotate_Shift (ROT_SLA, DST_H);
   procedure SLA_L is new Rotate_Shift (ROT_SLA, DST_L);
   procedure SLA_Addr_HL is new Rotate_Shift (ROT_SLA, DST_Addr_HL);

   --  SRA
   procedure SRA_A is new Rotate_Shift (ROT_SRA, DST_A);
   procedure SRA_B is new Rotate_Shift (ROT_SRA, DST_B);
   procedure SRA_C is new Rotate_Shift (ROT_SRA, DST_C);
   procedure SRA_D is new Rotate_Shift (ROT_SRA, DST_D);
   procedure SRA_E is new Rotate_Shift (ROT_SRA, DST_E);
   procedure SRA_H is new Rotate_Shift (ROT_SRA, DST_H);
   procedure SRA_L is new Rotate_Shift (ROT_SRA, DST_L);
   procedure SRA_Addr_HL is new Rotate_Shift (ROT_SRA, DST_Addr_HL);

   --  SWAP
   procedure SWAP_A is new Rotate_Shift (ROT_SWAP, DST_A);
   procedure SWAP_B is new Rotate_Shift (ROT_SWAP, DST_B);
   procedure SWAP_C is new Rotate_Shift (ROT_SWAP, DST_C);
   procedure SWAP_D is new Rotate_Shift (ROT_SWAP, DST_D);
   procedure SWAP_E is new Rotate_Shift (ROT_SWAP, DST_E);
   procedure SWAP_H is new Rotate_Shift (ROT_SWAP, DST_H);
   procedure SWAP_L is new Rotate_Shift (ROT_SWAP, DST_L);
   procedure SWAP_Addr_HL is new Rotate_Shift (ROT_SWAP, DST_Addr_HL);

   --  SRL
   procedure SRL_A is new Rotate_Shift (ROT_SRL, DST_A);
   procedure SRL_B is new Rotate_Shift (ROT_SRL, DST_B);
   procedure SRL_C is new Rotate_Shift (ROT_SRL, DST_C);
   procedure SRL_D is new Rotate_Shift (ROT_SRL, DST_D);
   procedure SRL_E is new Rotate_Shift (ROT_SRL, DST_E);
   procedure SRL_H is new Rotate_Shift (ROT_SRL, DST_H);
   procedure SRL_L is new Rotate_Shift (ROT_SRL, DST_L);
   procedure SRL_Addr_HL is new Rotate_Shift (ROT_SRL, DST_Addr_HL);

   --  BIT 0
   procedure BIT_0_A is new Bit_Source (BIT_Test, 0, SRC_A);
   procedure BIT_0_B is new Bit_Source (BIT_Test, 0, SRC_B);
   procedure BIT_0_C is new Bit_Source (BIT_Test, 0, SRC_C);
   procedure BIT_0_D is new Bit_Source (BIT_Test, 0, SRC_D);
   procedure BIT_0_E is new Bit_Source (BIT_Test, 0, SRC_E);
   procedure BIT_0_H is new Bit_Source (BIT_Test, 0, SRC_H);
   procedure BIT_0_L is new Bit_Source (BIT_Test, 0, SRC_L);
   procedure BIT_0_Addr_HL is new Bit_Source (BIT_Test, 0, SRC_Addr_HL);

   --  BIT 1
   procedure BIT_1_A is new Bit_Source (BIT_Test, 1, SRC_A);
   procedure BIT_1_B is new Bit_Source (BIT_Test, 1, SRC_B);
   procedure BIT_1_C is new Bit_Source (BIT_Test, 1, SRC_C);
   procedure BIT_1_D is new Bit_Source (BIT_Test, 1, SRC_D);
   procedure BIT_1_E is new Bit_Source (BIT_Test, 1, SRC_E);
   procedure BIT_1_H is new Bit_Source (BIT_Test, 1, SRC_H);
   procedure BIT_1_L is new Bit_Source (BIT_Test, 1, SRC_L);
   procedure BIT_1_Addr_HL is new Bit_Source (BIT_Test, 1, SRC_Addr_HL);

   --  BIT 2
   procedure BIT_2_A is new Bit_Source (BIT_Test, 2, SRC_A);
   procedure BIT_2_B is new Bit_Source (BIT_Test, 2, SRC_B);
   procedure BIT_2_C is new Bit_Source (BIT_Test, 2, SRC_C);
   procedure BIT_2_D is new Bit_Source (BIT_Test, 2, SRC_D);
   procedure BIT_2_E is new Bit_Source (BIT_Test, 2, SRC_E);
   procedure BIT_2_H is new Bit_Source (BIT_Test, 2, SRC_H);
   procedure BIT_2_L is new Bit_Source (BIT_Test, 2, SRC_L);
   procedure BIT_2_Addr_HL is new Bit_Source (BIT_Test, 2, SRC_Addr_HL);

   --  BIT 3
   procedure BIT_3_A is new Bit_Source (BIT_Test, 3, SRC_A);
   procedure BIT_3_B is new Bit_Source (BIT_Test, 3, SRC_B);
   procedure BIT_3_C is new Bit_Source (BIT_Test, 3, SRC_C);
   procedure BIT_3_D is new Bit_Source (BIT_Test, 3, SRC_D);
   procedure BIT_3_E is new Bit_Source (BIT_Test, 3, SRC_E);
   procedure BIT_3_H is new Bit_Source (BIT_Test, 3, SRC_H);
   procedure BIT_3_L is new Bit_Source (BIT_Test, 3, SRC_L);
   procedure BIT_3_Addr_HL is new Bit_Source (BIT_Test, 3, SRC_Addr_HL);

   --  BIT 4
   procedure BIT_4_A is new Bit_Source (BIT_Test, 4, SRC_A);
   procedure BIT_4_B is new Bit_Source (BIT_Test, 4, SRC_B);
   procedure BIT_4_C is new Bit_Source (BIT_Test, 4, SRC_C);
   procedure BIT_4_D is new Bit_Source (BIT_Test, 4, SRC_D);
   procedure BIT_4_E is new Bit_Source (BIT_Test, 4, SRC_E);
   procedure BIT_4_H is new Bit_Source (BIT_Test, 4, SRC_H);
   procedure BIT_4_L is new Bit_Source (BIT_Test, 4, SRC_L);
   procedure BIT_4_Addr_HL is new Bit_Source (BIT_Test, 4, SRC_Addr_HL);

   --  BIT 5
   procedure BIT_5_A is new Bit_Source (BIT_Test, 5, SRC_A);
   procedure BIT_5_B is new Bit_Source (BIT_Test, 5, SRC_B);
   procedure BIT_5_C is new Bit_Source (BIT_Test, 5, SRC_C);
   procedure BIT_5_D is new Bit_Source (BIT_Test, 5, SRC_D);
   procedure BIT_5_E is new Bit_Source (BIT_Test, 5, SRC_E);
   procedure BIT_5_H is new Bit_Source (BIT_Test, 5, SRC_H);
   procedure BIT_5_L is new Bit_Source (BIT_Test, 5, SRC_L);
   procedure BIT_5_Addr_HL is new Bit_Source (BIT_Test, 5, SRC_Addr_HL);

   --  BIT 6
   procedure BIT_6_A is new Bit_Source (BIT_Test, 6, SRC_A);
   procedure BIT_6_B is new Bit_Source (BIT_Test, 6, SRC_B);
   procedure BIT_6_C is new Bit_Source (BIT_Test, 6, SRC_C);
   procedure BIT_6_D is new Bit_Source (BIT_Test, 6, SRC_D);
   procedure BIT_6_E is new Bit_Source (BIT_Test, 6, SRC_E);
   procedure BIT_6_H is new Bit_Source (BIT_Test, 6, SRC_H);
   procedure BIT_6_L is new Bit_Source (BIT_Test, 6, SRC_L);
   procedure BIT_6_Addr_HL is new Bit_Source (BIT_Test, 6, SRC_Addr_HL);

   --  BIT 7
   procedure BIT_7_A is new Bit_Source (BIT_Test, 7, SRC_A);
   procedure BIT_7_B is new Bit_Source (BIT_Test, 7, SRC_B);
   procedure BIT_7_C is new Bit_Source (BIT_Test, 7, SRC_C);
   procedure BIT_7_D is new Bit_Source (BIT_Test, 7, SRC_D);
   procedure BIT_7_E is new Bit_Source (BIT_Test, 7, SRC_E);
   procedure BIT_7_H is new Bit_Source (BIT_Test, 7, SRC_H);
   procedure BIT_7_L is new Bit_Source (BIT_Test, 7, SRC_L);
   procedure BIT_7_Addr_HL is new Bit_Source (BIT_Test, 7, SRC_Addr_HL);

   --  RES 0
   procedure RES_0_A is new Bit_Source (BIT_Reset, 0, SRC_A);
   procedure RES_0_B is new Bit_Source (BIT_Reset, 0, SRC_B);
   procedure RES_0_C is new Bit_Source (BIT_Reset, 0, SRC_C);
   procedure RES_0_D is new Bit_Source (BIT_Reset, 0, SRC_D);
   procedure RES_0_E is new Bit_Source (BIT_Reset, 0, SRC_E);
   procedure RES_0_H is new Bit_Source (BIT_Reset, 0, SRC_H);
   procedure RES_0_L is new Bit_Source (BIT_Reset, 0, SRC_L);
   procedure RES_0_Addr_HL is new Bit_Source (BIT_Reset, 0, SRC_Addr_HL);

   --  RES 1
   procedure RES_1_A is new Bit_Source (BIT_Reset, 1, SRC_A);
   procedure RES_1_B is new Bit_Source (BIT_Reset, 1, SRC_B);
   procedure RES_1_C is new Bit_Source (BIT_Reset, 1, SRC_C);
   procedure RES_1_D is new Bit_Source (BIT_Reset, 1, SRC_D);
   procedure RES_1_E is new Bit_Source (BIT_Reset, 1, SRC_E);
   procedure RES_1_H is new Bit_Source (BIT_Reset, 1, SRC_H);
   procedure RES_1_L is new Bit_Source (BIT_Reset, 1, SRC_L);
   procedure RES_1_Addr_HL is new Bit_Source (BIT_Reset, 1, SRC_Addr_HL);

   --  RES 2
   procedure RES_2_A is new Bit_Source (BIT_Reset, 2, SRC_A);
   procedure RES_2_B is new Bit_Source (BIT_Reset, 2, SRC_B);
   procedure RES_2_C is new Bit_Source (BIT_Reset, 2, SRC_C);
   procedure RES_2_D is new Bit_Source (BIT_Reset, 2, SRC_D);
   procedure RES_2_E is new Bit_Source (BIT_Reset, 2, SRC_E);
   procedure RES_2_H is new Bit_Source (BIT_Reset, 2, SRC_H);
   procedure RES_2_L is new Bit_Source (BIT_Reset, 2, SRC_L);
   procedure RES_2_Addr_HL is new Bit_Source (BIT_Reset, 2, SRC_Addr_HL);

   --  RES 3
   procedure RES_3_A is new Bit_Source (BIT_Reset, 3, SRC_A);
   procedure RES_3_B is new Bit_Source (BIT_Reset, 3, SRC_B);
   procedure RES_3_C is new Bit_Source (BIT_Reset, 3, SRC_C);
   procedure RES_3_D is new Bit_Source (BIT_Reset, 3, SRC_D);
   procedure RES_3_E is new Bit_Source (BIT_Reset, 3, SRC_E);
   procedure RES_3_H is new Bit_Source (BIT_Reset, 3, SRC_H);
   procedure RES_3_L is new Bit_Source (BIT_Reset, 3, SRC_L);
   procedure RES_3_Addr_HL is new Bit_Source (BIT_Reset, 3, SRC_Addr_HL);

   --  RES 4
   procedure RES_4_A is new Bit_Source (BIT_Reset, 4, SRC_A);
   procedure RES_4_B is new Bit_Source (BIT_Reset, 4, SRC_B);
   procedure RES_4_C is new Bit_Source (BIT_Reset, 4, SRC_C);
   procedure RES_4_D is new Bit_Source (BIT_Reset, 4, SRC_D);
   procedure RES_4_E is new Bit_Source (BIT_Reset, 4, SRC_E);
   procedure RES_4_H is new Bit_Source (BIT_Reset, 4, SRC_H);
   procedure RES_4_L is new Bit_Source (BIT_Reset, 4, SRC_L);
   procedure RES_4_Addr_HL is new Bit_Source (BIT_Reset, 4, SRC_Addr_HL);

   --  RES 5
   procedure RES_5_A is new Bit_Source (BIT_Reset, 5, SRC_A);
   procedure RES_5_B is new Bit_Source (BIT_Reset, 5, SRC_B);
   procedure RES_5_C is new Bit_Source (BIT_Reset, 5, SRC_C);
   procedure RES_5_D is new Bit_Source (BIT_Reset, 5, SRC_D);
   procedure RES_5_E is new Bit_Source (BIT_Reset, 5, SRC_E);
   procedure RES_5_H is new Bit_Source (BIT_Reset, 5, SRC_H);
   procedure RES_5_L is new Bit_Source (BIT_Reset, 5, SRC_L);
   procedure RES_5_Addr_HL is new Bit_Source (BIT_Reset, 5, SRC_Addr_HL);

   --  RES 6
   procedure RES_6_A is new Bit_Source (BIT_Reset, 6, SRC_A);
   procedure RES_6_B is new Bit_Source (BIT_Reset, 6, SRC_B);
   procedure RES_6_C is new Bit_Source (BIT_Reset, 6, SRC_C);
   procedure RES_6_D is new Bit_Source (BIT_Reset, 6, SRC_D);
   procedure RES_6_E is new Bit_Source (BIT_Reset, 6, SRC_E);
   procedure RES_6_H is new Bit_Source (BIT_Reset, 6, SRC_H);
   procedure RES_6_L is new Bit_Source (BIT_Reset, 6, SRC_L);
   procedure RES_6_Addr_HL is new Bit_Source (BIT_Reset, 6, SRC_Addr_HL);

   --  RES 7
   procedure RES_7_A is new Bit_Source (BIT_Reset, 7, SRC_A);
   procedure RES_7_B is new Bit_Source (BIT_Reset, 7, SRC_B);
   procedure RES_7_C is new Bit_Source (BIT_Reset, 7, SRC_C);
   procedure RES_7_D is new Bit_Source (BIT_Reset, 7, SRC_D);
   procedure RES_7_E is new Bit_Source (BIT_Reset, 7, SRC_E);
   procedure RES_7_H is new Bit_Source (BIT_Reset, 7, SRC_H);
   procedure RES_7_L is new Bit_Source (BIT_Reset, 7, SRC_L);
   procedure RES_7_Addr_HL is new Bit_Source (BIT_Reset, 7, SRC_Addr_HL);

   --  SET 0
   procedure SET_0_A is new Bit_Source (BIT_Set, 0, SRC_A);
   procedure SET_0_B is new Bit_Source (BIT_Set, 0, SRC_B);
   procedure SET_0_C is new Bit_Source (BIT_Set, 0, SRC_C);
   procedure SET_0_D is new Bit_Source (BIT_Set, 0, SRC_D);
   procedure SET_0_E is new Bit_Source (BIT_Set, 0, SRC_E);
   procedure SET_0_H is new Bit_Source (BIT_Set, 0, SRC_H);
   procedure SET_0_L is new Bit_Source (BIT_Set, 0, SRC_L);
   procedure SET_0_Addr_HL is new Bit_Source (BIT_Set, 0, SRC_Addr_HL);

   --  SET 1
   procedure SET_1_A is new Bit_Source (BIT_Set, 1, SRC_A);
   procedure SET_1_B is new Bit_Source (BIT_Set, 1, SRC_B);
   procedure SET_1_C is new Bit_Source (BIT_Set, 1, SRC_C);
   procedure SET_1_D is new Bit_Source (BIT_Set, 1, SRC_D);
   procedure SET_1_E is new Bit_Source (BIT_Set, 1, SRC_E);
   procedure SET_1_H is new Bit_Source (BIT_Set, 1, SRC_H);
   procedure SET_1_L is new Bit_Source (BIT_Set, 1, SRC_L);
   procedure SET_1_Addr_HL is new Bit_Source (BIT_Set, 1, SRC_Addr_HL);

   --  SET 2
   procedure SET_2_A is new Bit_Source (BIT_Set, 2, SRC_A);
   procedure SET_2_B is new Bit_Source (BIT_Set, 2, SRC_B);
   procedure SET_2_C is new Bit_Source (BIT_Set, 2, SRC_C);
   procedure SET_2_D is new Bit_Source (BIT_Set, 2, SRC_D);
   procedure SET_2_E is new Bit_Source (BIT_Set, 2, SRC_E);
   procedure SET_2_H is new Bit_Source (BIT_Set, 2, SRC_H);
   procedure SET_2_L is new Bit_Source (BIT_Set, 2, SRC_L);
   procedure SET_2_Addr_HL is new Bit_Source (BIT_Set, 2, SRC_Addr_HL);

   --  SET 3
   procedure SET_3_A is new Bit_Source (BIT_Set, 3, SRC_A);
   procedure SET_3_B is new Bit_Source (BIT_Set, 3, SRC_B);
   procedure SET_3_C is new Bit_Source (BIT_Set, 3, SRC_C);
   procedure SET_3_D is new Bit_Source (BIT_Set, 3, SRC_D);
   procedure SET_3_E is new Bit_Source (BIT_Set, 3, SRC_E);
   procedure SET_3_H is new Bit_Source (BIT_Set, 3, SRC_H);
   procedure SET_3_L is new Bit_Source (BIT_Set, 3, SRC_L);
   procedure SET_3_Addr_HL is new Bit_Source (BIT_Set, 3, SRC_Addr_HL);

   --  SET 4
   procedure SET_4_A is new Bit_Source (BIT_Set, 4, SRC_A);
   procedure SET_4_B is new Bit_Source (BIT_Set, 4, SRC_B);
   procedure SET_4_C is new Bit_Source (BIT_Set, 4, SRC_C);
   procedure SET_4_D is new Bit_Source (BIT_Set, 4, SRC_D);
   procedure SET_4_E is new Bit_Source (BIT_Set, 4, SRC_E);
   procedure SET_4_H is new Bit_Source (BIT_Set, 4, SRC_H);
   procedure SET_4_L is new Bit_Source (BIT_Set, 4, SRC_L);
   procedure SET_4_Addr_HL is new Bit_Source (BIT_Set, 4, SRC_Addr_HL);

   --  SET 5
   procedure SET_5_A is new Bit_Source (BIT_Set, 5, SRC_A);
   procedure SET_5_B is new Bit_Source (BIT_Set, 5, SRC_B);
   procedure SET_5_C is new Bit_Source (BIT_Set, 5, SRC_C);
   procedure SET_5_D is new Bit_Source (BIT_Set, 5, SRC_D);
   procedure SET_5_E is new Bit_Source (BIT_Set, 5, SRC_E);
   procedure SET_5_H is new Bit_Source (BIT_Set, 5, SRC_H);
   procedure SET_5_L is new Bit_Source (BIT_Set, 5, SRC_L);
   procedure SET_5_Addr_HL is new Bit_Source (BIT_Set, 5, SRC_Addr_HL);

   --  SET 6
   procedure SET_6_A is new Bit_Source (BIT_Set, 6, SRC_A);
   procedure SET_6_B is new Bit_Source (BIT_Set, 6, SRC_B);
   procedure SET_6_C is new Bit_Source (BIT_Set, 6, SRC_C);
   procedure SET_6_D is new Bit_Source (BIT_Set, 6, SRC_D);
   procedure SET_6_E is new Bit_Source (BIT_Set, 6, SRC_E);
   procedure SET_6_H is new Bit_Source (BIT_Set, 6, SRC_H);
   procedure SET_6_L is new Bit_Source (BIT_Set, 6, SRC_L);
   procedure SET_6_Addr_HL is new Bit_Source (BIT_Set, 6, SRC_Addr_HL);

   --  SET 7
   procedure SET_7_A is new Bit_Source (BIT_Set, 7, SRC_A);
   procedure SET_7_B is new Bit_Source (BIT_Set, 7, SRC_B);
   procedure SET_7_C is new Bit_Source (BIT_Set, 7, SRC_C);
   procedure SET_7_D is new Bit_Source (BIT_Set, 7, SRC_D);
   procedure SET_7_E is new Bit_Source (BIT_Set, 7, SRC_E);
   procedure SET_7_H is new Bit_Source (BIT_Set, 7, SRC_H);
   procedure SET_7_L is new Bit_Source (BIT_Set, 7, SRC_L);
   procedure SET_7_Addr_HL is new Bit_Source (BIT_Set, 7, SRC_Addr_HL);

end Gade.Dev.CPU.Instructions.Bitwise.Instances;
