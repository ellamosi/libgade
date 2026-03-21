package body Gade.Dev.CPU.Instructions.Bitwise is

   procedure RLC (CPU : in out CPU_Context; Adjust_Flags : Boolean; Value : in out Byte)
   is
   begin
      Set_Value (CPU.Regs.F.C, (Value and 16#80#) /= 0);
      Value := Value * 2;
      Value := Value or CPU_Flag'Pos (CPU.Regs.F.C);
      Reset (CPU.Regs.F.H);
      Reset (CPU.Regs.F.N);
      Set_Value (CPU.Regs.F.Z, Adjust_Flags and Value = 0);
   end RLC;

   procedure RL (CPU : in out CPU_Context; Adjust_Flags : Boolean; Value : in out Byte) is
      Had_Carry : constant Boolean := Is_Set (CPU.Regs.F.C);
   begin
      Set_Value (CPU.Regs.F.C, (Value and 16#80#) /= 0);
      Value := Value * 2;
      Value := Value or Boolean'Pos (Had_Carry);
      Reset (CPU.Regs.F.H);
      Reset (CPU.Regs.F.N);
      Set_Value (CPU.Regs.F.Z, Adjust_Flags and Value = 0);
   end RL;

   procedure RRC (CPU : in out CPU_Context; Adjust_Flags : Boolean; Value : in out Byte)
   is
   begin
      Set_Value (CPU.Regs.F.C, (Value and 16#01#) /= 0);
      Value := Value / 2 or ((Value and 16#01#) * 2**7);
      Reset (CPU.Regs.F.H);
      Reset (CPU.Regs.F.N);
      Set_Value (CPU.Regs.F.Z, Adjust_Flags and Value = 0);
   end RRC;

   procedure RR (CPU : in out CPU_Context; Adjust_Flags : Boolean; Value : in out Byte) is
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
   end RR;

   procedure Shift_Left
     (CPU : in out CPU_Context; Arithmetic : Boolean; Value : in out Byte)
   is
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
   end Shift_Left;

   procedure Shift_Right
     (CPU : in out CPU_Context; Arithmetic : Boolean; Value : in out Byte)
   is
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
   end Shift_Right;

   procedure Set_Bit
     (CPU       : in out CPU_Context;
      Bit_Value : Bit;
      Index     : Instructions.Bit_Index;
      Value     : Byte;
      Result    : out Byte)
   is
      pragma Unreferenced (CPU);
      Mask : constant Byte := 2**Index;
   begin
      case Bit_Value is
         when 0 =>
            Result := Value and not Mask;

         when 1 =>
            Result := Value or Mask;
      end case;
   end Set_Bit;

   procedure Test_Bit
     (CPU : in out CPU_Context; Index : Instructions.Bit_Index; Value : Byte)
   is
      Mask : constant Byte := 2**Index;
   begin
      Reset (CPU.Regs.F.N);
      Set (CPU.Regs.F.H);
      Set_Value (CPU.Regs.F.Z, (Mask and Value) = 0);
   end Test_Bit;

   procedure Swap (CPU : in out CPU_Context; Value : in out Byte) is
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
   end Swap;

   procedure Rotate_Shift (GB : in out Gade.GB.GB_Type) is
      Value : Byte := Instructions.Load_Target (GB, Target);
   begin
      case Operation is
         when Instructions.ROT_RLC  =>
            RLC (GB.CPU, Adjust_Flags, Value);

         when Instructions.ROT_RRC  =>
            RRC (GB.CPU, Adjust_Flags, Value);

         when Instructions.ROT_RL   =>
            RL (GB.CPU, Adjust_Flags, Value);

         when Instructions.ROT_RR   =>
            RR (GB.CPU, Adjust_Flags, Value);

         when Instructions.ROT_SLA  =>
            Shift_Left (GB.CPU, S_A, Value);

         when Instructions.ROT_SRA  =>
            Shift_Right (GB.CPU, S_A, Value);

         when Instructions.ROT_SWAP =>
            Swap (GB.CPU, Value);

         when Instructions.ROT_SRL  =>
            Shift_Right (GB.CPU, S_L, Value);
      end case;

      Instructions.Store_Target (GB, Target, Value);
   end Rotate_Shift;

   procedure Bit_Source (GB : in out Gade.GB.GB_Type) is
      Value  : constant Byte := Instructions.Fetch_Source (GB, Target);
      Result : Byte;
   begin
      case Operation is
         when Instructions.BIT_Test  =>
            Test_Bit (GB.CPU, Index, Value);

         when Instructions.BIT_Set   =>
            Set_Bit (GB.CPU, SR_SET, Index, Value, Result);
            Instructions.Store_Target
              (GB,
               Instructions.Byte_Target_Kind'Val
                 (Instructions.Byte_Source_Kind'Pos (Target)),
               Result);

         when Instructions.BIT_Reset =>
            Set_Bit (GB.CPU, SR_RES, Index, Value, Result);
            Instructions.Store_Target
              (GB,
               Instructions.Byte_Target_Kind'Val
                 (Instructions.Byte_Source_Kind'Pos (Target)),
               Result);
      end case;
   end Bit_Source;

end Gade.Dev.CPU.Instructions.Bitwise;
