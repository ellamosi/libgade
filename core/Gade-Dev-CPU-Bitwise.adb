with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;

package body Gade.Dev.CPU.Bitwise is

   procedure Do_RLC
      (CPU          : in out CPU_Context;
       Adjust_Flags : Boolean;
       Value        : in out Byte) is
   begin
      -- Contains old bit 7 data.
      Set_Value(CPU.Regs.F.C, (Value and 16#80#) /= 0);

      Value := Value * 2;
      Value := Value or CPU_Flag'Pos(CPU.Regs.F.C);

      -- H is reset
      Reset(CPU.Regs.F.H);
      -- N is reset
      Reset(CPU.Regs.F.N);

      Set_Value(CPU.Regs.F.Z, Adjust_Flags and Value = 0);
   end Do_RLC;

   procedure Do_RL
      (CPU          : in out CPU_Context;
       Adjust_Flags : Boolean;
       Value        : in out Byte) is

      C2 : Boolean;
   begin
      C2 := Is_Set(CPU.Regs.F.C);
      -- C is data from bit 7 of Accumulator
      Set_Value(CPU.Regs.F.C, (Value and 16#80#) /= 0);
      Value := Value * 2;
      Value := Value or Boolean'Pos(C2);

      -- H is reset
      Reset(CPU.Regs.F.H);
      -- N is reset
      Reset(CPU.Regs.F.N);

      Set_Value(CPU.Regs.F.Z, Adjust_Flags and Value = 0);
   end Do_RL;

   procedure Do_RRC
      (CPU          : in out CPU_Context;
       Adjust_Flags : Boolean;
       Value        : in out Byte) is
   begin
      -- C is data from bit 0 of source register
      Set_Value(CPU.Regs.F.C, (Value and 16#01#) /= 0);

      Value := Value/2 or ((Value and 16#01#) * 2**7);

      -- H is reset
      Reset(CPU.Regs.F.H);
      -- N is reset
      Reset(CPU.Regs.F.N);

      Set_Value(CPU.Regs.F.Z, Adjust_Flags and Value = 0);
   end Do_RRC;

   procedure Do_RR
      (CPU          : in out CPU_Context;
       Adjust_Flags : Boolean;
       Value        : in out Byte) is
      Had_Carry : Boolean;
   begin
      Had_Carry := Is_Set(CPU.Regs.F.C);

      -- C - Contains old bit 0 data.
      Set_Value(CPU.Regs.F.C, (Value and 16#01#) /= 0);

      Value := Value / 2;
      if Had_Carry then Value := Value or 16#80#; end if;

      -- H is reset
      Reset(CPU.Regs.F.H);
      -- N is reset
      Reset(CPU.Regs.F.N);

      Set_Value(CPU.Regs.F.Z, Adjust_Flags and Value = 0);
--        -- Z - Set if result is zero.
--        Set_Value(CPU.Regs.F.Z, Value = 0);
   end Do_RR;

   procedure Do_SL
      (CPU        : in out CPU_Context;
       Arithmetic : Boolean;
       Value      : in out Byte) is

      Value_Native : Native_Unsigned;
   begin
      Value_Native := Native_Unsigned(Value);
      Set_Value(CPU.Regs.F.C, (Value_Native and 16#80#) /= 0);
      Value_Native := Value_Native * 2;

      if not Arithmetic then
         Value_Native := Value_Native or 1;
      end if;

      Set_Value(CPU.Regs.F.Z, (Value_Native and 16#FF#) = 0);
      Reset(CPU.Regs.F.H);
      Reset(CPU.Regs.F.N);

      Value := Byte(Value_Native and 16#FF#);
   end Do_SL;

   procedure Do_SR
      (CPU        : in out CPU_Context;
       Arithmetic : Boolean;
       Value      : in out Byte) is

      Value_Native : Native_Unsigned;
      High_Bit     : Native_Unsigned;
   begin
      Value_Native := Native_Unsigned(Value);
      High_Bit := Value_Native and 16#80#;

      Set_Value(CPU.Regs.F.C, (Value_Native and 16#01#) /= 0);
      Value_Native := Value_Native/2;

      if Arithmetic then
         Value_Native := Value_Native or High_Bit;
      end if;

      Set_Value(CPU.Regs.F.Z, Value_Native = 0);
      Reset(CPU.Regs.F.H);
      Reset(CPU.Regs.F.N);

      Value := Byte(Value_Native and 16#FF#);
   end Do_SR;

   procedure Do_Set_Bit
      (CPU          : in out CPU_Context;
       Bit_Value    : Bit;
       Index        : Bit_Index;
       Value        : Byte;
       Result       : out Byte) is
      Mask : Byte;
   begin
      Mask := 2**Index;
      case Bit_Value is
      when 0 =>
         Result := Value and not Mask;
      when 1 =>
         Result := Value or Mask;
      end case;
   end Do_Set_Bit;

   procedure Do_Bit
      (CPU          : in out CPU_Context;
       Index        : Bit_Index;
       Value        : Byte) is
      Mask : Byte;
   begin
      Reset(CPU.Regs.F.N);
      Set(CPU.Regs.F.H);
      Mask := 2**Index;
      Set_Value(CPU.Regs.F.Z, (Mask and Value) = 0);
   end;

   procedure Do_Swap
      (CPU        : in out CPU_Context;
       Value      : in out Byte) is

      Value_Native, Result_Native : Native_Unsigned;
      High_Nibble, Low_Nibble : Native_Unsigned;
   begin
      Value_Native := Native_Unsigned(Value);
      Low_Nibble := Value_Native and 16#0F#;
      High_Nibble := Value_Native / 2**4;
      Result_Native := Low_Nibble * 2**4 or High_Nibble;

      -- Z - Set if result is zero.
      Set_Value(CPU.Regs.F.Z, Result_Native = 0);
      -- N - Reset.
      Reset(CPU.Regs.F.N);
      -- H - Reset.
      Reset(CPU.Regs.F.H);
      -- C - Reset.
      Reset(CPU.Regs.F.C);

      Value := Byte(Result_Native);
   end;

end Gade.Dev.CPU.Bitwise;
