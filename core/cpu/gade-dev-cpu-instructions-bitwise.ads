package Gade.Dev.CPU.Instructions.Bitwise is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Do_RLC
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte);

   procedure Do_RL
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte);

   procedure Do_RRC
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte);

   procedure Do_RR
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte);

   SR_SET : constant Bit := 1;
   SR_RES : constant Bit := 0;

   procedure Do_Set_Bit
     (CPU       : in out CPU_Context;
      Bit_Value :        Bit;
      Index     :        Instructions.Bit_Index;
      Value     :        Byte;
      Result    :    out Byte);

   procedure Do_Bit
     (CPU   : in out CPU_Context;
      Index :        Instructions.Bit_Index;
      Value :        Byte);

   S_L : constant Boolean := False;
   S_A : constant Boolean := True;

   procedure Do_SL
     (CPU        : in out CPU_Context;
      Arithmetic :        Boolean;
      Value      : in out Byte);

   procedure Do_SR
     (CPU        : in out CPU_Context;
      Arithmetic :        Boolean;
      Value      : in out Byte);

   procedure Do_Swap
     (CPU   : in out CPU_Context;
      Value : in out Byte);

   generic
      Operation : Instructions.Bit_Op_Kind;
      Index     : Instructions.Bit_Index;
      Target    : Instructions.Byte_Source_Kind;
   procedure Execute_Bit_Source
     (GB : in out Gade.GB.GB_Type);

   generic
      Operation    : Instructions.Rotate_Shift_Op_Kind;
      Target       : Instructions.Byte_Target_Kind;
      Adjust_Flags : Boolean := True;
   procedure Execute_Rotate_Shift
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RLCA
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RLA
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RRCA
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RRA
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RLC_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RLC_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RLC_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RLC_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RLC_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RLC_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RLC_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RLC_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RRC_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RRC_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RRC_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RRC_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RRC_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RRC_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RL_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RRC_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RRC_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RL_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RL_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RL_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RL_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RL_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RL_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RL_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RR_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RR_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RR_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RR_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RR_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RR_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RR_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RR_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SLA_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SLA_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SLA_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SLA_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SLA_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SLA_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SLA_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SLA_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRA_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRA_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRA_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRA_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRA_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRA_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRA_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRA_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SWAP_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SWAP_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SWAP_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SWAP_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SWAP_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SWAP_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SWAP_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SWAP_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRL_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRL_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRL_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRL_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRL_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRL_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRL_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SRL_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_0_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_3_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_0_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_0_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_0_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_0_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_0_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_0_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_0_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_0_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_1_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_1_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_1_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_1_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_1_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_1_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_1_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_1_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_2_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_2_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_2_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_2_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_2_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_2_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_2_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_2_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_3_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_3_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_3_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_3_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_3_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_3_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_3_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_3_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_4_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_4_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_4_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_4_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_4_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_4_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_4_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_4_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_5_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_5_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_5_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_5_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_5_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_5_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_5_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_5_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_6_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_6_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_6_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_6_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_6_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_6_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_6_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_6_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_7_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_7_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_7_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_7_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_7_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_7_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_7_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_RES_7_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_0_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_0_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_0_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_0_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_0_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_0_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_0_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_0_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_1_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_1_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_1_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_1_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_1_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_1_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_1_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_1_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_2_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_2_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_2_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_2_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_2_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_2_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_2_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_2_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_3_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_3_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_3_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_3_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_3_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_3_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_3_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_3_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_4_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_4_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_4_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_4_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_4_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_4_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_4_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_4_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_5_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_5_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_5_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_5_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_5_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_5_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_5_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_5_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_6_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_6_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_6_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_6_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_6_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_6_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_6_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_6_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_7_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_7_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_7_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_7_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_7_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_7_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_7_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_SET_7_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_0_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_0_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_0_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_0_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_0_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_0_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_0_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_1_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_1_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_1_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_1_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_1_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_1_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_1_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_1_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_2_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_2_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_2_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_2_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_2_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_2_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_2_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_2_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_3_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_3_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_3_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_3_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_3_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_3_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_3_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_4_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_4_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_4_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_4_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_4_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_4_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_4_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_4_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_5_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_5_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_5_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_5_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_5_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_5_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_5_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_5_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_6_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_6_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_6_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_6_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_6_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_6_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_6_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_6_A
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_7_B
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_7_C
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_7_D
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_7_E
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_7_H
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_7_L
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_7_Addr_HL
     (GB : in out Gade.GB.GB_Type);
   procedure Execute_BIT_7_A
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions.Bitwise;
