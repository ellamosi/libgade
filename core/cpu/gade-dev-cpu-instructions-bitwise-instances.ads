package Gade.Dev.CPU.Instructions.Bitwise.Instances is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure RLCA is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation    => Instructions.ROT_RLC,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure RLA is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation    => Instructions.ROT_RL,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure RRCA is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation    => Instructions.ROT_RRC,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure RRA is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation    => Instructions.ROT_RR,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure RLC_B is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_B);

   procedure RLC_C is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_C);

   procedure RLC_D is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_D);

   procedure RLC_E is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_E);

   procedure RLC_H is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_H);

   procedure RLC_L is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_L);

   procedure RLC_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_Addr_HL);

   procedure RLC_A is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_A);

   procedure RRC_B is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_B);

   procedure RRC_C is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_C);

   procedure RRC_D is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_D);

   procedure RRC_E is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_E);

   procedure RRC_H is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_H);

   procedure RRC_L is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_L);

   procedure RL_C is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_C);

   procedure RRC_A is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_A);

   procedure RRC_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_Addr_HL);

   procedure RL_B is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_B);

   procedure RL_D is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_D);

   procedure RL_E is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_E);

   procedure RL_H is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_H);

   procedure RL_L is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_L);

   procedure RL_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_Addr_HL);

   procedure RL_A is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_A);

   procedure RR_B is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_B);

   procedure RR_C is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_C);

   procedure RR_D is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_D);

   procedure RR_E is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_E);

   procedure RR_H is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_H);

   procedure RR_L is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_L);

   procedure RR_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_Addr_HL);

   procedure RR_A is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_A);

   procedure SLA_B is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_B);

   procedure SLA_C is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_C);

   procedure SLA_D is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_D);

   procedure SLA_E is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_E);

   procedure SLA_H is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_H);

   procedure SLA_L is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_L);

   procedure SLA_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_Addr_HL);

   procedure SLA_A is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_A);

   procedure SRA_B is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_B);

   procedure SRA_C is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_C);

   procedure SRA_D is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_D);

   procedure SRA_E is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_E);

   procedure SRA_H is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_H);

   procedure SRA_L is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_L);

   procedure SRA_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_Addr_HL);

   procedure SRA_A is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_A);

   procedure SWAP_B is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_B);

   procedure SWAP_C is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_C);

   procedure SWAP_D is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_D);

   procedure SWAP_E is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_E);

   procedure SWAP_H is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_H);

   procedure SWAP_L is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_L);

   procedure SWAP_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_Addr_HL);

   procedure SWAP_A is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_A);

   procedure SRL_B is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_B);

   procedure SRL_C is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_C);

   procedure SRL_D is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_D);

   procedure SRL_E is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_E);

   procedure SRL_H is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_H);

   procedure SRL_L is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_L);

   procedure SRL_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_Addr_HL);

   procedure SRL_A is new Gade.Dev.CPU.Instructions.Bitwise.Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_A);

   procedure BIT_0_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test,
      Index     => 0,
      Target    => Instructions.SRC_B);

   procedure BIT_3_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test,
      Index     => 3,
      Target    => Instructions.SRC_Addr_HL);

   procedure RES_0_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_B);

   procedure RES_0_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_C);

   procedure RES_0_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_D);

   procedure RES_0_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_E);

   procedure RES_0_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_H);

   procedure RES_0_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_L);

   procedure RES_0_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_Addr_HL);

   procedure RES_0_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_A);

   procedure RES_1_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_B);

   procedure RES_1_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_C);

   procedure RES_1_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_D);

   procedure RES_1_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_E);

   procedure RES_1_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_H);

   procedure RES_1_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_L);

   procedure RES_1_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_Addr_HL);

   procedure RES_1_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_A);

   procedure RES_2_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_B);

   procedure RES_2_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_C);

   procedure RES_2_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_D);

   procedure RES_2_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_E);

   procedure RES_2_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_H);

   procedure RES_2_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_L);

   procedure RES_2_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_Addr_HL);

   procedure RES_2_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_A);

   procedure RES_3_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_B);

   procedure RES_3_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_C);

   procedure RES_3_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_D);

   procedure RES_3_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_E);

   procedure RES_3_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_H);

   procedure RES_3_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_L);

   procedure RES_3_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_Addr_HL);

   procedure RES_3_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_A);

   procedure RES_4_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_B);

   procedure RES_4_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_C);

   procedure RES_4_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_D);

   procedure RES_4_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_E);

   procedure RES_4_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_H);

   procedure RES_4_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_L);

   procedure RES_4_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_Addr_HL);

   procedure RES_4_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_A);

   procedure RES_5_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_B);

   procedure RES_5_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_C);

   procedure RES_5_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_D);

   procedure RES_5_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_E);

   procedure RES_5_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_H);

   procedure RES_5_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_L);

   procedure RES_5_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_Addr_HL);

   procedure RES_5_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_A);

   procedure RES_6_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_B);

   procedure RES_6_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_C);

   procedure RES_6_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_D);

   procedure RES_6_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_E);

   procedure RES_6_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_H);

   procedure RES_6_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_L);

   procedure RES_6_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_Addr_HL);

   procedure RES_6_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_A);

   procedure RES_7_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_B);

   procedure RES_7_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_C);

   procedure RES_7_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_D);

   procedure RES_7_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_E);

   procedure RES_7_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_H);

   procedure RES_7_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_L);

   procedure RES_7_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_Addr_HL);

   procedure RES_7_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_A);

   procedure SET_0_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_B);

   procedure SET_0_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_C);

   procedure SET_0_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_D);

   procedure SET_0_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_E);

   procedure SET_0_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_H);

   procedure SET_0_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_L);

   procedure SET_0_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_Addr_HL);

   procedure SET_0_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_A);

   procedure SET_1_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_B);

   procedure SET_1_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_C);

   procedure SET_1_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_D);

   procedure SET_1_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_E);

   procedure SET_1_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_H);

   procedure SET_1_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_L);

   procedure SET_1_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_Addr_HL);

   procedure SET_1_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_A);

   procedure SET_2_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_B);

   procedure SET_2_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_C);

   procedure SET_2_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_D);

   procedure SET_2_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_E);

   procedure SET_2_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_H);

   procedure SET_2_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_L);

   procedure SET_2_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_Addr_HL);

   procedure SET_2_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_A);

   procedure SET_3_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_B);

   procedure SET_3_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_C);

   procedure SET_3_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_D);

   procedure SET_3_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_E);

   procedure SET_3_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_H);

   procedure SET_3_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_L);

   procedure SET_3_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_Addr_HL);

   procedure SET_3_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_A);

   procedure SET_4_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_B);

   procedure SET_4_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_C);

   procedure SET_4_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_D);

   procedure SET_4_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_E);

   procedure SET_4_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_H);

   procedure SET_4_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_L);

   procedure SET_4_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_Addr_HL);

   procedure SET_4_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_A);

   procedure SET_5_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_B);

   procedure SET_5_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_C);

   procedure SET_5_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_D);

   procedure SET_5_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_E);

   procedure SET_5_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_H);

   procedure SET_5_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_L);

   procedure SET_5_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_Addr_HL);

   procedure SET_5_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_A);

   procedure SET_6_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_B);

   procedure SET_6_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_C);

   procedure SET_6_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_D);

   procedure SET_6_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_E);

   procedure SET_6_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_H);

   procedure SET_6_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_L);

   procedure SET_6_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_Addr_HL);

   procedure SET_6_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_A);

   procedure SET_7_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_B);

   procedure SET_7_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_C);

   procedure SET_7_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_D);

   procedure SET_7_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_E);

   procedure SET_7_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_H);

   procedure SET_7_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_L);

   procedure SET_7_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_Addr_HL);

   procedure SET_7_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_A);

   procedure BIT_0_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_C);

   procedure BIT_0_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_D);

   procedure BIT_0_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_E);

   procedure BIT_0_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_H);

   procedure BIT_0_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_L);

   procedure BIT_0_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_Addr_HL);

   procedure BIT_0_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_A);

   procedure BIT_1_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_B);

   procedure BIT_1_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_C);

   procedure BIT_1_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_D);

   procedure BIT_1_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_E);

   procedure BIT_1_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_H);

   procedure BIT_1_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_L);

   procedure BIT_1_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_Addr_HL);

   procedure BIT_1_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_A);

   procedure BIT_2_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_B);

   procedure BIT_2_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_C);

   procedure BIT_2_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_D);

   procedure BIT_2_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_E);

   procedure BIT_2_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_H);

   procedure BIT_2_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_L);

   procedure BIT_2_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_Addr_HL);

   procedure BIT_2_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_A);

   procedure BIT_3_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_B);

   procedure BIT_3_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_C);

   procedure BIT_3_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_D);

   procedure BIT_3_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_E);

   procedure BIT_3_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_H);

   procedure BIT_3_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_L);

   procedure BIT_3_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_A);

   procedure BIT_4_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_B);

   procedure BIT_4_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_C);

   procedure BIT_4_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_D);

   procedure BIT_4_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_E);

   procedure BIT_4_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_H);

   procedure BIT_4_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_L);

   procedure BIT_4_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_Addr_HL);

   procedure BIT_4_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_A);

   procedure BIT_5_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_B);

   procedure BIT_5_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_C);

   procedure BIT_5_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_D);

   procedure BIT_5_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_E);

   procedure BIT_5_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_H);

   procedure BIT_5_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_L);

   procedure BIT_5_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_Addr_HL);

   procedure BIT_5_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_A);

   procedure BIT_6_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_B);

   procedure BIT_6_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_C);

   procedure BIT_6_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_D);

   procedure BIT_6_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_E);

   procedure BIT_6_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_H);

   procedure BIT_6_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_L);

   procedure BIT_6_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_Addr_HL);

   procedure BIT_6_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_A);

   procedure BIT_7_B is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_B);

   procedure BIT_7_C is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_C);

   procedure BIT_7_D is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_D);

   procedure BIT_7_E is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_E);

   procedure BIT_7_H is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_H);

   procedure BIT_7_L is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_L);

   procedure BIT_7_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_Addr_HL);

   procedure BIT_7_A is new Gade.Dev.CPU.Instructions.Bitwise.Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_A);

end Gade.Dev.CPU.Instructions.Bitwise.Instances;
