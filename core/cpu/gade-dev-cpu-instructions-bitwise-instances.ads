package Gade.Dev.CPU.Instructions.Bitwise.Instances is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Execute_RLCA is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RLC,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RLA is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RL,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RRCA is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RRC,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RRA is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RR,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RLC_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_B);

   procedure Execute_RLC_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_C);

   procedure Execute_RLC_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_D);

   procedure Execute_RLC_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_E);

   procedure Execute_RLC_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_H);

   procedure Execute_RLC_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_L);

   procedure Execute_RLC_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RLC_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_A);

   procedure Execute_RRC_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_B);

   procedure Execute_RRC_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_C);

   procedure Execute_RRC_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_D);

   procedure Execute_RRC_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_E);

   procedure Execute_RRC_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_H);

   procedure Execute_RRC_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_L);

   procedure Execute_RL_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_C);

   procedure Execute_RRC_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_A);

   procedure Execute_RRC_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RL_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_B);

   procedure Execute_RL_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_D);

   procedure Execute_RL_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_E);

   procedure Execute_RL_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_H);

   procedure Execute_RL_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_L);

   procedure Execute_RL_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RL_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_A);

   procedure Execute_RR_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_B);

   procedure Execute_RR_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_C);

   procedure Execute_RR_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_D);

   procedure Execute_RR_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_E);

   procedure Execute_RR_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_H);

   procedure Execute_RR_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_L);

   procedure Execute_RR_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RR_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_A);

   procedure Execute_SLA_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_B);

   procedure Execute_SLA_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_C);

   procedure Execute_SLA_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_D);

   procedure Execute_SLA_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_E);

   procedure Execute_SLA_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_H);

   procedure Execute_SLA_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_L);

   procedure Execute_SLA_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SLA_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_A);

   procedure Execute_SRA_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_B);

   procedure Execute_SRA_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_C);

   procedure Execute_SRA_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_D);

   procedure Execute_SRA_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_E);

   procedure Execute_SRA_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_H);

   procedure Execute_SRA_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_L);

   procedure Execute_SRA_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SRA_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_A);

   procedure Execute_SWAP_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_B);

   procedure Execute_SWAP_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_C);

   procedure Execute_SWAP_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_D);

   procedure Execute_SWAP_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_E);

   procedure Execute_SWAP_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_H);

   procedure Execute_SWAP_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_L);

   procedure Execute_SWAP_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SWAP_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_A);

   procedure Execute_SRL_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_B);

   procedure Execute_SRL_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_C);

   procedure Execute_SRL_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_D);

   procedure Execute_SRL_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_E);

   procedure Execute_SRL_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_H);

   procedure Execute_SRL_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_L);

   procedure Execute_SRL_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SRL_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_A);

   procedure Execute_BIT_0_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test,
      Index     => 0,
      Target    => Instructions.SRC_B);

   procedure Execute_BIT_3_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test,
      Index     => 3,
      Target    => Instructions.SRC_Addr_HL);

   procedure Execute_RES_0_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_B);

   procedure Execute_RES_0_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_C);

   procedure Execute_RES_0_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_D);

   procedure Execute_RES_0_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_E);

   procedure Execute_RES_0_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_H);

   procedure Execute_RES_0_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_L);

   procedure Execute_RES_0_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_0_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_A);

   procedure Execute_RES_1_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_B);

   procedure Execute_RES_1_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_C);

   procedure Execute_RES_1_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_D);

   procedure Execute_RES_1_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_E);

   procedure Execute_RES_1_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_H);

   procedure Execute_RES_1_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_L);

   procedure Execute_RES_1_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_1_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_A);

   procedure Execute_RES_2_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_B);

   procedure Execute_RES_2_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_C);

   procedure Execute_RES_2_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_D);

   procedure Execute_RES_2_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_E);

   procedure Execute_RES_2_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_H);

   procedure Execute_RES_2_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_L);

   procedure Execute_RES_2_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_2_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_A);

   procedure Execute_RES_3_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_B);

   procedure Execute_RES_3_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_C);

   procedure Execute_RES_3_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_D);

   procedure Execute_RES_3_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_E);

   procedure Execute_RES_3_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_H);

   procedure Execute_RES_3_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_L);

   procedure Execute_RES_3_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_3_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_A);

   procedure Execute_RES_4_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_B);

   procedure Execute_RES_4_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_C);

   procedure Execute_RES_4_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_D);

   procedure Execute_RES_4_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_E);

   procedure Execute_RES_4_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_H);

   procedure Execute_RES_4_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_L);

   procedure Execute_RES_4_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_4_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_A);

   procedure Execute_RES_5_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_B);

   procedure Execute_RES_5_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_C);

   procedure Execute_RES_5_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_D);

   procedure Execute_RES_5_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_E);

   procedure Execute_RES_5_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_H);

   procedure Execute_RES_5_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_L);

   procedure Execute_RES_5_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_5_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_A);

   procedure Execute_RES_6_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_B);

   procedure Execute_RES_6_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_C);

   procedure Execute_RES_6_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_D);

   procedure Execute_RES_6_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_E);

   procedure Execute_RES_6_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_H);

   procedure Execute_RES_6_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_L);

   procedure Execute_RES_6_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_6_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_A);

   procedure Execute_RES_7_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_B);

   procedure Execute_RES_7_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_C);

   procedure Execute_RES_7_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_D);

   procedure Execute_RES_7_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_E);

   procedure Execute_RES_7_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_H);

   procedure Execute_RES_7_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_L);

   procedure Execute_RES_7_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_7_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_A);

   procedure Execute_SET_0_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_B);

   procedure Execute_SET_0_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_C);

   procedure Execute_SET_0_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_D);

   procedure Execute_SET_0_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_E);

   procedure Execute_SET_0_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_H);

   procedure Execute_SET_0_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_L);

   procedure Execute_SET_0_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_0_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_A);

   procedure Execute_SET_1_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_B);

   procedure Execute_SET_1_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_C);

   procedure Execute_SET_1_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_D);

   procedure Execute_SET_1_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_E);

   procedure Execute_SET_1_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_H);

   procedure Execute_SET_1_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_L);

   procedure Execute_SET_1_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_1_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_A);

   procedure Execute_SET_2_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_B);

   procedure Execute_SET_2_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_C);

   procedure Execute_SET_2_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_D);

   procedure Execute_SET_2_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_E);

   procedure Execute_SET_2_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_H);

   procedure Execute_SET_2_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_L);

   procedure Execute_SET_2_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_2_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_A);

   procedure Execute_SET_3_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_B);

   procedure Execute_SET_3_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_C);

   procedure Execute_SET_3_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_D);

   procedure Execute_SET_3_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_E);

   procedure Execute_SET_3_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_H);

   procedure Execute_SET_3_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_L);

   procedure Execute_SET_3_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_3_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_A);

   procedure Execute_SET_4_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_B);

   procedure Execute_SET_4_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_C);

   procedure Execute_SET_4_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_D);

   procedure Execute_SET_4_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_E);

   procedure Execute_SET_4_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_H);

   procedure Execute_SET_4_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_L);

   procedure Execute_SET_4_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_4_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_A);

   procedure Execute_SET_5_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_B);

   procedure Execute_SET_5_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_C);

   procedure Execute_SET_5_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_D);

   procedure Execute_SET_5_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_E);

   procedure Execute_SET_5_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_H);

   procedure Execute_SET_5_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_L);

   procedure Execute_SET_5_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_5_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_A);

   procedure Execute_SET_6_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_B);

   procedure Execute_SET_6_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_C);

   procedure Execute_SET_6_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_D);

   procedure Execute_SET_6_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_E);

   procedure Execute_SET_6_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_H);

   procedure Execute_SET_6_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_L);

   procedure Execute_SET_6_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_6_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_A);

   procedure Execute_SET_7_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_B);

   procedure Execute_SET_7_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_C);

   procedure Execute_SET_7_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_D);

   procedure Execute_SET_7_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_E);

   procedure Execute_SET_7_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_H);

   procedure Execute_SET_7_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_L);

   procedure Execute_SET_7_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_7_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_A);

   procedure Execute_BIT_0_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_C);

   procedure Execute_BIT_0_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_D);

   procedure Execute_BIT_0_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_E);

   procedure Execute_BIT_0_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_H);

   procedure Execute_BIT_0_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_L);

   procedure Execute_BIT_0_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_0_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_A);

   procedure Execute_BIT_1_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_B);

   procedure Execute_BIT_1_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_C);

   procedure Execute_BIT_1_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_D);

   procedure Execute_BIT_1_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_E);

   procedure Execute_BIT_1_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_H);

   procedure Execute_BIT_1_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_L);

   procedure Execute_BIT_1_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_1_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_A);

   procedure Execute_BIT_2_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_B);

   procedure Execute_BIT_2_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_C);

   procedure Execute_BIT_2_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_D);

   procedure Execute_BIT_2_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_E);

   procedure Execute_BIT_2_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_H);

   procedure Execute_BIT_2_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_L);

   procedure Execute_BIT_2_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_2_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_A);

   procedure Execute_BIT_3_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_B);

   procedure Execute_BIT_3_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_C);

   procedure Execute_BIT_3_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_D);

   procedure Execute_BIT_3_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_E);

   procedure Execute_BIT_3_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_H);

   procedure Execute_BIT_3_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_L);

   procedure Execute_BIT_3_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_A);

   procedure Execute_BIT_4_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_B);

   procedure Execute_BIT_4_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_C);

   procedure Execute_BIT_4_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_D);

   procedure Execute_BIT_4_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_E);

   procedure Execute_BIT_4_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_H);

   procedure Execute_BIT_4_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_L);

   procedure Execute_BIT_4_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_4_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_A);

   procedure Execute_BIT_5_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_B);

   procedure Execute_BIT_5_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_C);

   procedure Execute_BIT_5_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_D);

   procedure Execute_BIT_5_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_E);

   procedure Execute_BIT_5_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_H);

   procedure Execute_BIT_5_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_L);

   procedure Execute_BIT_5_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_5_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_A);

   procedure Execute_BIT_6_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_B);

   procedure Execute_BIT_6_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_C);

   procedure Execute_BIT_6_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_D);

   procedure Execute_BIT_6_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_E);

   procedure Execute_BIT_6_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_H);

   procedure Execute_BIT_6_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_L);

   procedure Execute_BIT_6_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_6_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_A);

   procedure Execute_BIT_7_B is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_B);

   procedure Execute_BIT_7_C is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_C);

   procedure Execute_BIT_7_D is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_D);

   procedure Execute_BIT_7_E is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_E);

   procedure Execute_BIT_7_H is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_H);

   procedure Execute_BIT_7_L is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_L);

   procedure Execute_BIT_7_Addr_HL is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_7_A is new Gade.Dev.CPU.Instructions.Bitwise.Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_A);

end Gade.Dev.CPU.Instructions.Bitwise.Instances;
