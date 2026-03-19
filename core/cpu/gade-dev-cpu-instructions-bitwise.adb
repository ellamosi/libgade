package body Gade.Dev.CPU.Instructions.Bitwise is

   procedure Do_RLC
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte) is
   begin
      Set_Value (CPU.Regs.F.C, (Value and 16#80#) /= 0);
      Value := Value * 2;
      Value := Value or CPU_Flag'Pos (CPU.Regs.F.C);
      Reset (CPU.Regs.F.H);
      Reset (CPU.Regs.F.N);
      Set_Value (CPU.Regs.F.Z, Adjust_Flags and Value = 0);
   end Do_RLC;

   procedure Do_RL
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte) is
      Had_Carry : constant Boolean := Is_Set (CPU.Regs.F.C);
   begin
      Set_Value (CPU.Regs.F.C, (Value and 16#80#) /= 0);
      Value := Value * 2;
      Value := Value or Boolean'Pos (Had_Carry);
      Reset (CPU.Regs.F.H);
      Reset (CPU.Regs.F.N);
      Set_Value (CPU.Regs.F.Z, Adjust_Flags and Value = 0);
   end Do_RL;

   procedure Do_RRC
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte) is
   begin
      Set_Value (CPU.Regs.F.C, (Value and 16#01#) /= 0);
      Value := Value / 2 or ((Value and 16#01#) * 2**7);
      Reset (CPU.Regs.F.H);
      Reset (CPU.Regs.F.N);
      Set_Value (CPU.Regs.F.Z, Adjust_Flags and Value = 0);
   end Do_RRC;

   procedure Do_RR
     (CPU          : in out CPU_Context;
      Adjust_Flags :        Boolean;
      Value        : in out Byte) is
      Had_Carry : constant Boolean := Is_Set (CPU.Regs.F.C);
   begin
      Set_Value (CPU.Regs.F.C, (Value and 16#01#) /= 0);
      Value := Value / 2;
      if Had_Carry then
         Value := Value or 16#80#;
      end if;
      Reset (CPU.Regs.F.H);
      Reset (CPU.Regs.F.N);
      Set_Value (CPU.Regs.F.Z, Adjust_Flags and Value = 0);
   end Do_RR;

   procedure Do_SL
     (CPU        : in out CPU_Context;
      Arithmetic :        Boolean;
      Value      : in out Byte) is
      Value_Native : Native_Unsigned := Native_Unsigned (Value);
   begin
      Set_Value (CPU.Regs.F.C, (Value_Native and 16#80#) /= 0);
      Value_Native := Value_Native * 2;

      if not Arithmetic then
         Value_Native := Value_Native or 1;
      end if;

      Set_Value (CPU.Regs.F.Z, (Value_Native and 16#FF#) = 0);
      Reset (CPU.Regs.F.H);
      Reset (CPU.Regs.F.N);

      Value := Byte (Value_Native and 16#FF#);
   end Do_SL;

   procedure Do_SR
     (CPU        : in out CPU_Context;
      Arithmetic :        Boolean;
      Value      : in out Byte) is
      Value_Native : Native_Unsigned := Native_Unsigned (Value);
      High_Bit     : constant Native_Unsigned := Value_Native and 16#80#;
   begin
      Set_Value (CPU.Regs.F.C, (Value_Native and 16#01#) /= 0);
      Value_Native := Value_Native / 2;

      if Arithmetic then
         Value_Native := Value_Native or High_Bit;
      end if;

      Set_Value (CPU.Regs.F.Z, Value_Native = 0);
      Reset (CPU.Regs.F.H);
      Reset (CPU.Regs.F.N);

      Value := Byte (Value_Native and 16#FF#);
   end Do_SR;

   procedure Do_Set_Bit
     (CPU       : in out CPU_Context;
      Bit_Value :        Bit;
      Index     :        Instructions.Bit_Index;
      Value     :        Byte;
      Result    :    out Byte) is
      pragma Unreferenced (CPU);
      Mask : constant Byte := 2**Index;
   begin
      case Bit_Value is
         when 0 =>
            Result := Value and not Mask;
         when 1 =>
            Result := Value or Mask;
      end case;
   end Do_Set_Bit;

   procedure Do_Bit
     (CPU   : in out CPU_Context;
      Index :        Instructions.Bit_Index;
      Value :        Byte) is
      Mask : constant Byte := 2**Index;
   begin
      Reset (CPU.Regs.F.N);
      Set (CPU.Regs.F.H);
      Set_Value (CPU.Regs.F.Z, (Mask and Value) = 0);
   end Do_Bit;

   procedure Do_Swap
     (CPU   : in out CPU_Context;
      Value : in out Byte) is
      Value_Native  : constant Native_Unsigned := Native_Unsigned (Value);
      Low_Nibble    : constant Native_Unsigned := Value_Native and 16#0F#;
      High_Nibble   : constant Native_Unsigned := Value_Native / 2**4;
      Result_Native : constant Native_Unsigned := Low_Nibble * 2**4 or High_Nibble;
   begin
      Set_Value (CPU.Regs.F.Z, Result_Native = 0);
      Reset (CPU.Regs.F.N);
      Reset (CPU.Regs.F.H);
      Reset (CPU.Regs.F.C);
      Value := Byte (Result_Native);
   end Do_Swap;


   procedure Execute_Rotate_Shift
     (GB : in out Gade.GB.GB_Type) is
      Value : Byte := Instructions.Load_Target (GB, Target);
   begin
      case Operation is
         when Instructions.ROT_RLC =>
            Do_RLC (GB.CPU, Adjust_Flags, Value);
         when Instructions.ROT_RRC =>
            Do_RRC (GB.CPU, Adjust_Flags, Value);
         when Instructions.ROT_RL =>
            Do_RL (GB.CPU, Adjust_Flags, Value);
         when Instructions.ROT_RR =>
            Do_RR (GB.CPU, Adjust_Flags, Value);
         when Instructions.ROT_SLA =>
            Do_SL (GB.CPU, S_A, Value);
         when Instructions.ROT_SRA =>
            Do_SR (GB.CPU, S_A, Value);
         when Instructions.ROT_SWAP =>
            Do_Swap (GB.CPU, Value);
         when Instructions.ROT_SRL =>
            Do_SR (GB.CPU, S_L, Value);
      end case;

      Instructions.Store_Target (GB, Target, Value);
   end Execute_Rotate_Shift;


   procedure Execute_Bit_Source
     (GB : in out Gade.GB.GB_Type) is
      Value  : constant Byte := Instructions.Fetch_Source (GB, Target);
      Result : Byte;
   begin
      case Operation is
         when Instructions.BIT_Test =>
            Do_Bit (GB.CPU, Index, Value);
         when Instructions.BIT_Set =>
            Do_Set_Bit (GB.CPU, SR_SET, Index, Value, Result);
            Instructions.Store_Target
              (GB, Instructions.Byte_Target_Kind'Val (Instructions.Byte_Source_Kind'Pos (Target)), Result);
         when Instructions.BIT_Reset =>
            Do_Set_Bit (GB.CPU, SR_RES, Index, Value, Result);
            Instructions.Store_Target
              (GB, Instructions.Byte_Target_Kind'Val (Instructions.Byte_Source_Kind'Pos (Target)), Result);
      end case;
   end Execute_Bit_Source;

   procedure Execute_RLCA_Impl is new Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RLC,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RLA_Impl is new Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RL,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RRCA_Impl is new Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RRC,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RRA_Impl is new Execute_Rotate_Shift
     (Operation    => Instructions.ROT_RR,
      Target       => Instructions.DST_A,
      Adjust_Flags => False);

   procedure Execute_RLC_B_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_B);

   procedure Execute_RLC_C_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_C);

   procedure Execute_RLC_D_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_D);

   procedure Execute_RLC_E_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_E);

   procedure Execute_RLC_H_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_H);

   procedure Execute_RLC_L_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_L);

   procedure Execute_RLC_Addr_HL_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RLC_A_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RLC,
      Target    => Instructions.DST_A);

   procedure Execute_RRC_B_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_B);

   procedure Execute_RRC_C_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_C);

   procedure Execute_RRC_D_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_D);

   procedure Execute_RRC_E_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_E);

   procedure Execute_RRC_H_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_H);

   procedure Execute_RRC_L_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_L);

   procedure Execute_RL_C_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_C);

   procedure Execute_RRC_A_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_A);

   procedure Execute_RRC_Addr_HL_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RRC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RL_B_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_B);

   procedure Execute_RL_D_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_D);

   procedure Execute_RL_E_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_E);

   procedure Execute_RL_H_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_H);

   procedure Execute_RL_L_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_L);

   procedure Execute_RL_Addr_HL_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RL_A_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RL,
      Target    => Instructions.DST_A);

   procedure Execute_RR_B_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_B);

   procedure Execute_RR_C_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_C);

   procedure Execute_RR_D_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_D);

   procedure Execute_RR_E_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_E);

   procedure Execute_RR_H_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_H);

   procedure Execute_RR_L_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_L);

   procedure Execute_RR_Addr_HL_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_RR_A_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_RR,
      Target    => Instructions.DST_A);

   procedure Execute_SLA_B_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_B);

   procedure Execute_SLA_C_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_C);

   procedure Execute_SLA_D_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_D);

   procedure Execute_SLA_E_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_E);

   procedure Execute_SLA_H_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_H);

   procedure Execute_SLA_L_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_L);

   procedure Execute_SLA_Addr_HL_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SLA_A_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SLA,
      Target    => Instructions.DST_A);

   procedure Execute_SRA_B_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_B);

   procedure Execute_SRA_C_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_C);

   procedure Execute_SRA_D_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_D);

   procedure Execute_SRA_E_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_E);

   procedure Execute_SRA_H_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_H);

   procedure Execute_SRA_L_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_L);

   procedure Execute_SRA_Addr_HL_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SRA_A_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRA,
      Target    => Instructions.DST_A);

   procedure Execute_SWAP_B_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_B);

   procedure Execute_SWAP_C_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_C);

   procedure Execute_SWAP_D_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_D);

   procedure Execute_SWAP_E_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_E);

   procedure Execute_SWAP_H_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_H);

   procedure Execute_SWAP_L_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_L);

   procedure Execute_SWAP_Addr_HL_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SWAP_A_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SWAP,
      Target    => Instructions.DST_A);

   procedure Execute_SRL_B_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_B);

   procedure Execute_SRL_C_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_C);

   procedure Execute_SRL_D_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_D);

   procedure Execute_SRL_E_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_E);

   procedure Execute_SRL_H_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_H);

   procedure Execute_SRL_L_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_L);

   procedure Execute_SRL_Addr_HL_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_SRL_A_Impl is new Execute_Rotate_Shift
     (Operation => Instructions.ROT_SRL,
      Target    => Instructions.DST_A);

   procedure Execute_BIT_0_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test,
      Index     => 0,
      Target    => Instructions.SRC_B);

   procedure Execute_BIT_3_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test,
      Index     => 3,
      Target    => Instructions.SRC_Addr_HL);

   procedure Execute_RES_0_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_B);

   procedure Execute_RES_0_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_C);

   procedure Execute_RES_0_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_D);

   procedure Execute_RES_0_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_E);

   procedure Execute_RES_0_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_H);

   procedure Execute_RES_0_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_L);

   procedure Execute_RES_0_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_0_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 0, Target => Instructions.SRC_A);

   procedure Execute_RES_1_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_B);

   procedure Execute_RES_1_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_C);

   procedure Execute_RES_1_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_D);

   procedure Execute_RES_1_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_E);

   procedure Execute_RES_1_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_H);

   procedure Execute_RES_1_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_L);

   procedure Execute_RES_1_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_1_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 1, Target => Instructions.SRC_A);

   procedure Execute_RES_2_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_B);

   procedure Execute_RES_2_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_C);

   procedure Execute_RES_2_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_D);

   procedure Execute_RES_2_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_E);

   procedure Execute_RES_2_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_H);

   procedure Execute_RES_2_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_L);

   procedure Execute_RES_2_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_2_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 2, Target => Instructions.SRC_A);

   procedure Execute_RES_3_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_B);

   procedure Execute_RES_3_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_C);

   procedure Execute_RES_3_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_D);

   procedure Execute_RES_3_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_E);

   procedure Execute_RES_3_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_H);

   procedure Execute_RES_3_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_L);

   procedure Execute_RES_3_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_3_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 3, Target => Instructions.SRC_A);

   procedure Execute_RES_4_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_B);

   procedure Execute_RES_4_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_C);

   procedure Execute_RES_4_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_D);

   procedure Execute_RES_4_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_E);

   procedure Execute_RES_4_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_H);

   procedure Execute_RES_4_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_L);

   procedure Execute_RES_4_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_4_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 4, Target => Instructions.SRC_A);

   procedure Execute_RES_5_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_B);

   procedure Execute_RES_5_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_C);

   procedure Execute_RES_5_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_D);

   procedure Execute_RES_5_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_E);

   procedure Execute_RES_5_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_H);

   procedure Execute_RES_5_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_L);

   procedure Execute_RES_5_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_5_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 5, Target => Instructions.SRC_A);

   procedure Execute_RES_6_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_B);

   procedure Execute_RES_6_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_C);

   procedure Execute_RES_6_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_D);

   procedure Execute_RES_6_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_E);

   procedure Execute_RES_6_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_H);

   procedure Execute_RES_6_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_L);

   procedure Execute_RES_6_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_6_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 6, Target => Instructions.SRC_A);

   procedure Execute_RES_7_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_B);

   procedure Execute_RES_7_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_C);

   procedure Execute_RES_7_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_D);

   procedure Execute_RES_7_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_E);

   procedure Execute_RES_7_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_H);

   procedure Execute_RES_7_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_L);

   procedure Execute_RES_7_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_Addr_HL);

   procedure Execute_RES_7_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Reset, Index => 7, Target => Instructions.SRC_A);

   procedure Execute_SET_0_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_B);

   procedure Execute_SET_0_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_C);

   procedure Execute_SET_0_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_D);

   procedure Execute_SET_0_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_E);

   procedure Execute_SET_0_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_H);

   procedure Execute_SET_0_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_L);

   procedure Execute_SET_0_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_0_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 0, Target => Instructions.SRC_A);

   procedure Execute_SET_1_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_B);

   procedure Execute_SET_1_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_C);

   procedure Execute_SET_1_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_D);

   procedure Execute_SET_1_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_E);

   procedure Execute_SET_1_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_H);

   procedure Execute_SET_1_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_L);

   procedure Execute_SET_1_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_1_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 1, Target => Instructions.SRC_A);

   procedure Execute_SET_2_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_B);

   procedure Execute_SET_2_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_C);

   procedure Execute_SET_2_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_D);

   procedure Execute_SET_2_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_E);

   procedure Execute_SET_2_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_H);

   procedure Execute_SET_2_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_L);

   procedure Execute_SET_2_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_2_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 2, Target => Instructions.SRC_A);

   procedure Execute_SET_3_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_B);

   procedure Execute_SET_3_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_C);

   procedure Execute_SET_3_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_D);

   procedure Execute_SET_3_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_E);

   procedure Execute_SET_3_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_H);

   procedure Execute_SET_3_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_L);

   procedure Execute_SET_3_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_3_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 3, Target => Instructions.SRC_A);

   procedure Execute_SET_4_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_B);

   procedure Execute_SET_4_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_C);

   procedure Execute_SET_4_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_D);

   procedure Execute_SET_4_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_E);

   procedure Execute_SET_4_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_H);

   procedure Execute_SET_4_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_L);

   procedure Execute_SET_4_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_4_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 4, Target => Instructions.SRC_A);

   procedure Execute_SET_5_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_B);

   procedure Execute_SET_5_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_C);

   procedure Execute_SET_5_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_D);

   procedure Execute_SET_5_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_E);

   procedure Execute_SET_5_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_H);

   procedure Execute_SET_5_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_L);

   procedure Execute_SET_5_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_5_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 5, Target => Instructions.SRC_A);

   procedure Execute_SET_6_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_B);

   procedure Execute_SET_6_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_C);

   procedure Execute_SET_6_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_D);

   procedure Execute_SET_6_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_E);

   procedure Execute_SET_6_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_H);

   procedure Execute_SET_6_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_L);

   procedure Execute_SET_6_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_6_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 6, Target => Instructions.SRC_A);

   procedure Execute_SET_7_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_B);

   procedure Execute_SET_7_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_C);

   procedure Execute_SET_7_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_D);

   procedure Execute_SET_7_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_E);

   procedure Execute_SET_7_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_H);

   procedure Execute_SET_7_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_L);

   procedure Execute_SET_7_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_Addr_HL);

   procedure Execute_SET_7_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Set, Index => 7, Target => Instructions.SRC_A);

   procedure Execute_BIT_0_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_C);

   procedure Execute_BIT_0_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_D);

   procedure Execute_BIT_0_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_E);

   procedure Execute_BIT_0_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_H);

   procedure Execute_BIT_0_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_L);

   procedure Execute_BIT_0_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_0_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 0, Target => Instructions.SRC_A);

   procedure Execute_BIT_1_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_B);

   procedure Execute_BIT_1_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_C);

   procedure Execute_BIT_1_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_D);

   procedure Execute_BIT_1_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_E);

   procedure Execute_BIT_1_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_H);

   procedure Execute_BIT_1_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_L);

   procedure Execute_BIT_1_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_1_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 1, Target => Instructions.SRC_A);

   procedure Execute_BIT_2_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_B);

   procedure Execute_BIT_2_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_C);

   procedure Execute_BIT_2_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_D);

   procedure Execute_BIT_2_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_E);

   procedure Execute_BIT_2_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_H);

   procedure Execute_BIT_2_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_L);

   procedure Execute_BIT_2_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_2_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 2, Target => Instructions.SRC_A);

   procedure Execute_BIT_3_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_B);

   procedure Execute_BIT_3_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_C);

   procedure Execute_BIT_3_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_D);

   procedure Execute_BIT_3_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_E);

   procedure Execute_BIT_3_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_H);

   procedure Execute_BIT_3_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_L);

   procedure Execute_BIT_3_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 3, Target => Instructions.SRC_A);

   procedure Execute_BIT_4_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_B);

   procedure Execute_BIT_4_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_C);

   procedure Execute_BIT_4_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_D);

   procedure Execute_BIT_4_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_E);

   procedure Execute_BIT_4_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_H);

   procedure Execute_BIT_4_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_L);

   procedure Execute_BIT_4_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_4_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 4, Target => Instructions.SRC_A);

   procedure Execute_BIT_5_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_B);

   procedure Execute_BIT_5_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_C);

   procedure Execute_BIT_5_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_D);

   procedure Execute_BIT_5_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_E);

   procedure Execute_BIT_5_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_H);

   procedure Execute_BIT_5_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_L);

   procedure Execute_BIT_5_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_5_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 5, Target => Instructions.SRC_A);

   procedure Execute_BIT_6_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_B);

   procedure Execute_BIT_6_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_C);

   procedure Execute_BIT_6_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_D);

   procedure Execute_BIT_6_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_E);

   procedure Execute_BIT_6_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_H);

   procedure Execute_BIT_6_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_L);

   procedure Execute_BIT_6_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_6_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 6, Target => Instructions.SRC_A);

   procedure Execute_BIT_7_B_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_B);

   procedure Execute_BIT_7_C_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_C);

   procedure Execute_BIT_7_D_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_D);

   procedure Execute_BIT_7_E_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_E);

   procedure Execute_BIT_7_H_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_H);

   procedure Execute_BIT_7_L_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_L);

   procedure Execute_BIT_7_Addr_HL_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_Addr_HL);

   procedure Execute_BIT_7_A_Impl is new Execute_Bit_Source
     (Operation => Instructions.BIT_Test, Index => 7, Target => Instructions.SRC_A);

   procedure Execute_RLCA
     (GB : in out Gade.GB.GB_Type) renames Execute_RLCA_Impl;
   procedure Execute_RLA
     (GB : in out Gade.GB.GB_Type) renames Execute_RLA_Impl;
   procedure Execute_RRCA
     (GB : in out Gade.GB.GB_Type) renames Execute_RRCA_Impl;
   procedure Execute_RRA
     (GB : in out Gade.GB.GB_Type) renames Execute_RRA_Impl;
   procedure Execute_RLC_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RLC_B_Impl;
   procedure Execute_RLC_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RLC_C_Impl;
   procedure Execute_RLC_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RLC_D_Impl;
   procedure Execute_RLC_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RLC_E_Impl;
   procedure Execute_RLC_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RLC_H_Impl;
   procedure Execute_RLC_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RLC_L_Impl;
   procedure Execute_RLC_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RLC_Addr_HL_Impl;
   procedure Execute_RLC_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RLC_A_Impl;
   procedure Execute_RRC_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RRC_B_Impl;
   procedure Execute_RRC_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RRC_C_Impl;
   procedure Execute_RRC_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RRC_D_Impl;
   procedure Execute_RRC_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RRC_E_Impl;
   procedure Execute_RRC_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RRC_H_Impl;
   procedure Execute_RRC_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RRC_L_Impl;
   procedure Execute_RL_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RL_C_Impl;
   procedure Execute_RRC_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RRC_A_Impl;
   procedure Execute_RRC_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RRC_Addr_HL_Impl;
   procedure Execute_RL_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RL_B_Impl;
   procedure Execute_RL_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RL_D_Impl;
   procedure Execute_RL_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RL_E_Impl;
   procedure Execute_RL_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RL_H_Impl;
   procedure Execute_RL_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RL_L_Impl;
   procedure Execute_RL_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RL_Addr_HL_Impl;
   procedure Execute_RL_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RL_A_Impl;
   procedure Execute_RR_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RR_B_Impl;
   procedure Execute_RR_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RR_C_Impl;
   procedure Execute_RR_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RR_D_Impl;
   procedure Execute_RR_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RR_E_Impl;
   procedure Execute_RR_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RR_H_Impl;
   procedure Execute_RR_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RR_L_Impl;
   procedure Execute_RR_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RR_Addr_HL_Impl;
   procedure Execute_RR_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RR_A_Impl;
   procedure Execute_SLA_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SLA_B_Impl;
   procedure Execute_SLA_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SLA_C_Impl;
   procedure Execute_SLA_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SLA_D_Impl;
   procedure Execute_SLA_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SLA_E_Impl;
   procedure Execute_SLA_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SLA_H_Impl;
   procedure Execute_SLA_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SLA_L_Impl;
   procedure Execute_SLA_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SLA_Addr_HL_Impl;
   procedure Execute_SLA_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SLA_A_Impl;
   procedure Execute_SRA_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SRA_B_Impl;
   procedure Execute_SRA_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SRA_C_Impl;
   procedure Execute_SRA_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SRA_D_Impl;
   procedure Execute_SRA_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SRA_E_Impl;
   procedure Execute_SRA_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SRA_H_Impl;
   procedure Execute_SRA_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SRA_L_Impl;
   procedure Execute_SRA_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SRA_Addr_HL_Impl;
   procedure Execute_SRA_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SRA_A_Impl;
   procedure Execute_SWAP_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SWAP_B_Impl;
   procedure Execute_SWAP_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SWAP_C_Impl;
   procedure Execute_SWAP_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SWAP_D_Impl;
   procedure Execute_SWAP_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SWAP_E_Impl;
   procedure Execute_SWAP_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SWAP_H_Impl;
   procedure Execute_SWAP_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SWAP_L_Impl;
   procedure Execute_SWAP_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SWAP_Addr_HL_Impl;
   procedure Execute_SWAP_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SWAP_A_Impl;
   procedure Execute_SRL_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SRL_B_Impl;
   procedure Execute_SRL_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SRL_C_Impl;
   procedure Execute_SRL_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SRL_D_Impl;
   procedure Execute_SRL_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SRL_E_Impl;
   procedure Execute_SRL_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SRL_H_Impl;
   procedure Execute_SRL_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SRL_L_Impl;
   procedure Execute_SRL_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SRL_Addr_HL_Impl;
   procedure Execute_SRL_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SRL_A_Impl;
   procedure Execute_BIT_0_B
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_0_B_Impl;
   procedure Execute_BIT_3_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_3_Addr_HL_Impl;
   procedure Execute_RES_0_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_0_B_Impl;
   procedure Execute_RES_0_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_0_C_Impl;
   procedure Execute_RES_0_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_0_D_Impl;
   procedure Execute_RES_0_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_0_E_Impl;
   procedure Execute_RES_0_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_0_H_Impl;
   procedure Execute_RES_0_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_0_L_Impl;
   procedure Execute_RES_0_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_0_Addr_HL_Impl;
   procedure Execute_RES_0_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_0_A_Impl;
   procedure Execute_RES_1_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_1_B_Impl;
   procedure Execute_RES_1_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_1_C_Impl;
   procedure Execute_RES_1_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_1_D_Impl;
   procedure Execute_RES_1_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_1_E_Impl;
   procedure Execute_RES_1_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_1_H_Impl;
   procedure Execute_RES_1_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_1_L_Impl;
   procedure Execute_RES_1_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_1_Addr_HL_Impl;
   procedure Execute_RES_1_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_1_A_Impl;
   procedure Execute_RES_2_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_2_B_Impl;
   procedure Execute_RES_2_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_2_C_Impl;
   procedure Execute_RES_2_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_2_D_Impl;
   procedure Execute_RES_2_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_2_E_Impl;
   procedure Execute_RES_2_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_2_H_Impl;
   procedure Execute_RES_2_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_2_L_Impl;
   procedure Execute_RES_2_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_2_Addr_HL_Impl;
   procedure Execute_RES_2_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_2_A_Impl;
   procedure Execute_RES_3_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_3_B_Impl;
   procedure Execute_RES_3_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_3_C_Impl;
   procedure Execute_RES_3_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_3_D_Impl;
   procedure Execute_RES_3_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_3_E_Impl;
   procedure Execute_RES_3_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_3_H_Impl;
   procedure Execute_RES_3_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_3_L_Impl;
   procedure Execute_RES_3_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_3_Addr_HL_Impl;
   procedure Execute_RES_3_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_3_A_Impl;
   procedure Execute_RES_4_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_4_B_Impl;
   procedure Execute_RES_4_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_4_C_Impl;
   procedure Execute_RES_4_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_4_D_Impl;
   procedure Execute_RES_4_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_4_E_Impl;
   procedure Execute_RES_4_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_4_H_Impl;
   procedure Execute_RES_4_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_4_L_Impl;
   procedure Execute_RES_4_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_4_Addr_HL_Impl;
   procedure Execute_RES_4_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_4_A_Impl;
   procedure Execute_RES_5_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_5_B_Impl;
   procedure Execute_RES_5_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_5_C_Impl;
   procedure Execute_RES_5_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_5_D_Impl;
   procedure Execute_RES_5_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_5_E_Impl;
   procedure Execute_RES_5_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_5_H_Impl;
   procedure Execute_RES_5_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_5_L_Impl;
   procedure Execute_RES_5_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_5_Addr_HL_Impl;
   procedure Execute_RES_5_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_5_A_Impl;
   procedure Execute_RES_6_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_6_B_Impl;
   procedure Execute_RES_6_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_6_C_Impl;
   procedure Execute_RES_6_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_6_D_Impl;
   procedure Execute_RES_6_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_6_E_Impl;
   procedure Execute_RES_6_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_6_H_Impl;
   procedure Execute_RES_6_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_6_L_Impl;
   procedure Execute_RES_6_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_6_Addr_HL_Impl;
   procedure Execute_RES_6_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_6_A_Impl;
   procedure Execute_RES_7_B
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_7_B_Impl;
   procedure Execute_RES_7_C
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_7_C_Impl;
   procedure Execute_RES_7_D
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_7_D_Impl;
   procedure Execute_RES_7_E
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_7_E_Impl;
   procedure Execute_RES_7_H
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_7_H_Impl;
   procedure Execute_RES_7_L
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_7_L_Impl;
   procedure Execute_RES_7_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_7_Addr_HL_Impl;
   procedure Execute_RES_7_A
     (GB : in out Gade.GB.GB_Type) renames Execute_RES_7_A_Impl;
   procedure Execute_SET_0_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_0_B_Impl;
   procedure Execute_SET_0_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_0_C_Impl;
   procedure Execute_SET_0_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_0_D_Impl;
   procedure Execute_SET_0_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_0_E_Impl;
   procedure Execute_SET_0_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_0_H_Impl;
   procedure Execute_SET_0_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_0_L_Impl;
   procedure Execute_SET_0_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_0_Addr_HL_Impl;
   procedure Execute_SET_0_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_0_A_Impl;
   procedure Execute_SET_1_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_1_B_Impl;
   procedure Execute_SET_1_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_1_C_Impl;
   procedure Execute_SET_1_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_1_D_Impl;
   procedure Execute_SET_1_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_1_E_Impl;
   procedure Execute_SET_1_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_1_H_Impl;
   procedure Execute_SET_1_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_1_L_Impl;
   procedure Execute_SET_1_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_1_Addr_HL_Impl;
   procedure Execute_SET_1_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_1_A_Impl;
   procedure Execute_SET_2_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_2_B_Impl;
   procedure Execute_SET_2_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_2_C_Impl;
   procedure Execute_SET_2_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_2_D_Impl;
   procedure Execute_SET_2_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_2_E_Impl;
   procedure Execute_SET_2_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_2_H_Impl;
   procedure Execute_SET_2_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_2_L_Impl;
   procedure Execute_SET_2_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_2_Addr_HL_Impl;
   procedure Execute_SET_2_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_2_A_Impl;
   procedure Execute_SET_3_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_3_B_Impl;
   procedure Execute_SET_3_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_3_C_Impl;
   procedure Execute_SET_3_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_3_D_Impl;
   procedure Execute_SET_3_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_3_E_Impl;
   procedure Execute_SET_3_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_3_H_Impl;
   procedure Execute_SET_3_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_3_L_Impl;
   procedure Execute_SET_3_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_3_Addr_HL_Impl;
   procedure Execute_SET_3_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_3_A_Impl;
   procedure Execute_SET_4_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_4_B_Impl;
   procedure Execute_SET_4_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_4_C_Impl;
   procedure Execute_SET_4_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_4_D_Impl;
   procedure Execute_SET_4_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_4_E_Impl;
   procedure Execute_SET_4_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_4_H_Impl;
   procedure Execute_SET_4_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_4_L_Impl;
   procedure Execute_SET_4_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_4_Addr_HL_Impl;
   procedure Execute_SET_4_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_4_A_Impl;
   procedure Execute_SET_5_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_5_B_Impl;
   procedure Execute_SET_5_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_5_C_Impl;
   procedure Execute_SET_5_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_5_D_Impl;
   procedure Execute_SET_5_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_5_E_Impl;
   procedure Execute_SET_5_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_5_H_Impl;
   procedure Execute_SET_5_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_5_L_Impl;
   procedure Execute_SET_5_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_5_Addr_HL_Impl;
   procedure Execute_SET_5_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_5_A_Impl;
   procedure Execute_SET_6_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_6_B_Impl;
   procedure Execute_SET_6_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_6_C_Impl;
   procedure Execute_SET_6_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_6_D_Impl;
   procedure Execute_SET_6_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_6_E_Impl;
   procedure Execute_SET_6_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_6_H_Impl;
   procedure Execute_SET_6_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_6_L_Impl;
   procedure Execute_SET_6_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_6_Addr_HL_Impl;
   procedure Execute_SET_6_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_6_A_Impl;
   procedure Execute_SET_7_B
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_7_B_Impl;
   procedure Execute_SET_7_C
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_7_C_Impl;
   procedure Execute_SET_7_D
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_7_D_Impl;
   procedure Execute_SET_7_E
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_7_E_Impl;
   procedure Execute_SET_7_H
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_7_H_Impl;
   procedure Execute_SET_7_L
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_7_L_Impl;
   procedure Execute_SET_7_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_7_Addr_HL_Impl;
   procedure Execute_SET_7_A
     (GB : in out Gade.GB.GB_Type) renames Execute_SET_7_A_Impl;
   procedure Execute_BIT_0_C
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_0_C_Impl;
   procedure Execute_BIT_0_D
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_0_D_Impl;
   procedure Execute_BIT_0_E
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_0_E_Impl;
   procedure Execute_BIT_0_H
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_0_H_Impl;
   procedure Execute_BIT_0_L
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_0_L_Impl;
   procedure Execute_BIT_0_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_0_Addr_HL_Impl;
   procedure Execute_BIT_0_A
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_0_A_Impl;
   procedure Execute_BIT_1_B
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_1_B_Impl;
   procedure Execute_BIT_1_C
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_1_C_Impl;
   procedure Execute_BIT_1_D
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_1_D_Impl;
   procedure Execute_BIT_1_E
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_1_E_Impl;
   procedure Execute_BIT_1_H
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_1_H_Impl;
   procedure Execute_BIT_1_L
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_1_L_Impl;
   procedure Execute_BIT_1_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_1_Addr_HL_Impl;
   procedure Execute_BIT_1_A
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_1_A_Impl;
   procedure Execute_BIT_2_B
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_2_B_Impl;
   procedure Execute_BIT_2_C
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_2_C_Impl;
   procedure Execute_BIT_2_D
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_2_D_Impl;
   procedure Execute_BIT_2_E
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_2_E_Impl;
   procedure Execute_BIT_2_H
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_2_H_Impl;
   procedure Execute_BIT_2_L
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_2_L_Impl;
   procedure Execute_BIT_2_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_2_Addr_HL_Impl;
   procedure Execute_BIT_2_A
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_2_A_Impl;
   procedure Execute_BIT_3_B
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_3_B_Impl;
   procedure Execute_BIT_3_C
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_3_C_Impl;
   procedure Execute_BIT_3_D
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_3_D_Impl;
   procedure Execute_BIT_3_E
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_3_E_Impl;
   procedure Execute_BIT_3_H
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_3_H_Impl;
   procedure Execute_BIT_3_L
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_3_L_Impl;
   procedure Execute_BIT_3_A
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_3_A_Impl;
   procedure Execute_BIT_4_B
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_4_B_Impl;
   procedure Execute_BIT_4_C
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_4_C_Impl;
   procedure Execute_BIT_4_D
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_4_D_Impl;
   procedure Execute_BIT_4_E
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_4_E_Impl;
   procedure Execute_BIT_4_H
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_4_H_Impl;
   procedure Execute_BIT_4_L
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_4_L_Impl;
   procedure Execute_BIT_4_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_4_Addr_HL_Impl;
   procedure Execute_BIT_4_A
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_4_A_Impl;
   procedure Execute_BIT_5_B
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_5_B_Impl;
   procedure Execute_BIT_5_C
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_5_C_Impl;
   procedure Execute_BIT_5_D
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_5_D_Impl;
   procedure Execute_BIT_5_E
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_5_E_Impl;
   procedure Execute_BIT_5_H
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_5_H_Impl;
   procedure Execute_BIT_5_L
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_5_L_Impl;
   procedure Execute_BIT_5_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_5_Addr_HL_Impl;
   procedure Execute_BIT_5_A
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_5_A_Impl;
   procedure Execute_BIT_6_B
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_6_B_Impl;
   procedure Execute_BIT_6_C
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_6_C_Impl;
   procedure Execute_BIT_6_D
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_6_D_Impl;
   procedure Execute_BIT_6_E
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_6_E_Impl;
   procedure Execute_BIT_6_H
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_6_H_Impl;
   procedure Execute_BIT_6_L
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_6_L_Impl;
   procedure Execute_BIT_6_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_6_Addr_HL_Impl;
   procedure Execute_BIT_6_A
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_6_A_Impl;
   procedure Execute_BIT_7_B
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_7_B_Impl;
   procedure Execute_BIT_7_C
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_7_C_Impl;
   procedure Execute_BIT_7_D
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_7_D_Impl;
   procedure Execute_BIT_7_E
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_7_E_Impl;
   procedure Execute_BIT_7_H
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_7_H_Impl;
   procedure Execute_BIT_7_L
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_7_L_Impl;
   procedure Execute_BIT_7_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_7_Addr_HL_Impl;
   procedure Execute_BIT_7_A
     (GB : in out Gade.GB.GB_Type) renames Execute_BIT_7_A_Impl;

end Gade.Dev.CPU.Instructions.Bitwise;
