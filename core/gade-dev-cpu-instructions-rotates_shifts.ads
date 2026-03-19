package Gade.Dev.CPU.Instructions.Rotates_Shifts is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Execute_RLCA is new Instructions.Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RLC,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RLA is new Instructions.Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RL,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RRCA is new Instructions.Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RRC,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RRA is new Instructions.Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RR,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RLC_B is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_B);

   procedure Execute_RLC_C is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_C);

   procedure Execute_RLC_D is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_D);

   procedure Execute_RLC_E is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_E);

   procedure Execute_RLC_H is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_H);

   procedure Execute_RLC_L is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_L);

   procedure Execute_RLC_Addr_HL is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RLC_A is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_A);

   procedure Execute_RRC_B is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_B);

   procedure Execute_RRC_C is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_C);

   procedure Execute_RRC_D is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_D);

   procedure Execute_RRC_E is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_E);

   procedure Execute_RRC_H is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_H);

   procedure Execute_RRC_L is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_L);

   procedure Execute_RL_C is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_C);

   procedure Execute_RRC_A is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_A);

   procedure Execute_RRC_Addr_HL is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RL_B is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_B);

   procedure Execute_RL_D is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_D);

   procedure Execute_RL_E is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_E);

   procedure Execute_RL_H is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_H);

   procedure Execute_RL_L is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_L);

   procedure Execute_RL_Addr_HL is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RL_A is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_A);

   procedure Execute_RR_B is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_B);

   procedure Execute_RR_C is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_C);

   procedure Execute_RR_D is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_D);

   procedure Execute_RR_E is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_E);

   procedure Execute_RR_H is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_H);

   procedure Execute_RR_L is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_L);

   procedure Execute_RR_Addr_HL is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RR_A is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_A);

   procedure Execute_SLA_B is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_B);

   procedure Execute_SLA_C is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_C);

   procedure Execute_SLA_D is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_D);

   procedure Execute_SLA_E is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_E);

   procedure Execute_SLA_H is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_H);

   procedure Execute_SLA_L is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_L);

   procedure Execute_SLA_Addr_HL is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SLA_A is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_A);

   procedure Execute_SRA_B is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_B);

   procedure Execute_SRA_C is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_C);

   procedure Execute_SRA_D is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_D);

   procedure Execute_SRA_E is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_E);

   procedure Execute_SRA_H is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_H);

   procedure Execute_SRA_L is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_L);

   procedure Execute_SRA_Addr_HL is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SRA_A is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_A);

   procedure Execute_SWAP_B is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_B);

   procedure Execute_SWAP_C is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_C);

   procedure Execute_SWAP_D is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_D);

   procedure Execute_SWAP_E is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_E);

   procedure Execute_SWAP_H is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_H);

   procedure Execute_SWAP_L is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_L);

   procedure Execute_SWAP_Addr_HL is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SWAP_A is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_A);

   procedure Execute_SRL_B is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_B);

   procedure Execute_SRL_C is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_C);

   procedure Execute_SRL_D is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_D);

   procedure Execute_SRL_E is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_E);

   procedure Execute_SRL_H is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_H);

   procedure Execute_SRL_L is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_L);

   procedure Execute_SRL_Addr_HL is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SRL_A is new Instructions.Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_A);

end Gade.Dev.CPU.Instructions.Rotates_Shifts;
