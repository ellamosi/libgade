with Interfaces; use Interfaces;

package body Gade.Dev.CPU.Arithmetic is

   procedure Do_Add
     (CPU    : in out CPU_Context;
      Value  : Byte;
      Result : out Byte;
      Carry  : Carry_Type) is

      A_Native      : Native_Unsigned;
      Value_Native  : Native_Unsigned;
      Result_Native : Native_Unsigned;
      Carry_Value   : Native_Unsigned;
   begin
      A_Native := Native_Unsigned(CPU.Regs.A);
      Value_Native := Native_Unsigned(Value);

      -- N: Reset
      Reset(CPU.Regs.F.N);

      if Boolean(Carry) and Is_Set(CPU.Regs.F.C) then
         Carry_Value := 1;
      else
         Carry_Value := 0;
      end if;

      -- H : Set if carry from bit 3; reset otherwise
      Set_Value
        (CPU.Regs.F.H,
         (((A_Native and 16#0F#) + (Value_Native and 16#0F#) + Carry_Value)
          and 16#10#) /= 0);
      Result_Native := A_Native + Value_Native + Carry_Value;

      -- Set Byte result
      Result := Byte(Result_Native and 16#FF#);

      -- Z : Set if result is zero; reset otherwise
      Set_Value(CPU.Regs.F.Z, Result = 0);
      -- C : Set if carry from bit 7; reset otherwise
      Set_Value(CPU.Regs.F.C, (Result_Native and 16#100#) /= 0);
   end Do_Add;

   procedure Do_Add
     (CPU   : in out CPU_Context;
      Reg   : in out Word;
      Value : Byte) is
      -- Might be able to re-use the Add_Offset method for this
      Reg_Native    : Native_Unsigned;
      Value_Native  : Native_Unsigned;
      Result_Native : Native_Unsigned;
   begin
      Reg_Native := Native_Unsigned(Reg);
      Value_Native := Native_Unsigned(Value);

      -- N: Reset
      Reset(CPU.Regs.F.N);
      -- Z : Reset
      Reset(CPU.Regs.F.Z);

      if (Value_Native and 16#80#) /= 0 then
         Value_Native := Value_Native or 16#FF00#; -- Sign extension
      end if;

      -- H : Set if carry/borrow from bit 3; reset otherwise
      Set_Value
        (CPU.Regs.F.H,
         (((Reg_Native and 16#000F#) + (Value_Native and 16#0F#))
          and 16#10#) /= 0);
      Result_Native := Reg_Native + Value_Native;
      -- C : Set if carry/borrow from bit 7; reset otherwise
      Set_Value(CPU.Regs.F.C,
                (((Reg_Native and 16#00FF#) + (Value_Native and 16#00FF#))
                 and 16#100#) /= 0);

      -- Set Word result
      Reg := Word(Result_Native and 16#FFFF#);
   end Do_Add;

   procedure Do_Add
     (CPU    : in out CPU_Context;
      Value  : Word;
      Result : out Word) is

      HL_Native     : Native_Unsigned;
      Value_Native  : Native_Unsigned;
      Result_Native : Native_Unsigned;
   begin
      HL_Native := Native_Unsigned(CPU.Regs.HL);
      Value_Native := Native_Unsigned(Value);

      -- N: Reset
      Reset(CPU.Regs.F.N);

      -- H : Set if carry from bit 11; reset otherwise
      Set_Value
        (CPU.Regs.F.H,
         (((HL_Native and 16#0FFF#) + (Value_Native and 16#0FFF#))
          and 16#1000#) /= 0);
      Result_Native := HL_Native + Value_Native;

      -- Set Word result
      Result := Word(Result_Native and 16#FFFF#);

      -- Z : Unaffected
      -- C : Set if carry from bit 15; reset otherwise
      Set_Value(CPU.Regs.F.C, (Result_Native and 16#10000#) /= 0);
   end Do_Add;


   procedure Do_Sub
     (CPU    : in out CPU_Context;
      Value  : Byte;
      Result : out Byte;
      Carry  : Carry_Type) is

      A_Native      : Native_Unsigned;
      Value_Native  : Native_Unsigned;
      Result_Native : Native_Unsigned;
      Carry_Value   : Native_Unsigned;
   begin
      A_Native := Native_Unsigned(CPU.Regs.A);
      Value_Native := Native_Unsigned(Value);

      -- N: Set
      Set(CPU.Regs.F.N);

      if Boolean(Carry) and Is_Set(CPU.Regs.F.C) then
         Carry_Value := 1;
      else
         Carry_Value := 0;
      end if;

      -- H : Set if borrow from bit 4; reset otherwise
      Set_Value
        (CPU.Regs.F.H,
         (((A_Native and 16#0F#) - (Value_Native and 16#0F#) - Carry_Value)
          and 16#10#) /= 0);
      Result_Native := A_Native - Value_Native - Carry_Value;

      -- Set Byte result
      Result := Byte(Result_Native and 16#FF#);
      -- Z : Set if result is zero; reset otherwise
      Set_Value(CPU.Regs.F.Z, Result = 0);
      -- C : Set if borrow; reset otherwise
      Set_Value(CPU.Regs.F.C, (Result_Native and 16#100#) /= 0);
   end Do_Sub;

   procedure Add_Offset
     (CPU       : in out CPU_Context;
      Address   : in out Word;
      Offset    : Byte;
      Set_Flags : Boolean) is
      Result_Native  : Native_Unsigned;
      Address_Native : Native_Unsigned;
      Offset_Native  : Native_Unsigned;
   begin
      Address_Native := Native_Unsigned(Address);
      Offset_Native := Native_Unsigned(Offset);
      if (Offset_Native and 16#80#) = 0  then
         Result_Native := Address_Native + Offset_Native;
         if Set_Flags then
            -- H : Half-Carry is ignored (although some sources state otherwise)

            -- C : Set if carry from bit 15; reset otherwise
            Set_Value(CPU.Regs.F.C, (Result_Native and 16#10000#) /= 0);
         end if;
      else
         Result_Native := Address_Native - ((not Offset_Native) and 16#7F#) - 1;
         if Set_Flags then
            -- H : Half-Carry is ignored (although some sources state otherwise)

            -- C : Set if borrow; reset otherwise
            Set_Value(CPU.Regs.F.C, (Result_Native and 16#10000#) /= 0);
         end if;
      end if;
      Address := Word(Result_Native and 16#FFFF#);
   end Add_Offset;

   procedure Do_Inc_Dec
     (CPU     : in out CPU_Context;
      Inc_Dec : Inc_Dec_Type;
      Value   : Byte;
      Result  : out Byte) is
   begin
      if Inc_Dec = DEC then
         Result := Value - 1;
         Set_Value(CPU.Regs.F.H, (Value and 16#0F#) = 16#0#);
      else
         Result := Value + 1;
         Set_Value(CPU.Regs.F.H, (Value and 16#0F#) = 16#F#);
      end if;

      Set_Value(CPU.Regs.F.Z, Result = 0);
      Set_Value(CPU.Regs.F.N, Inc_Dec = DEC);
   end Do_Inc_Dec;

   procedure Do_Daa
     (CPU     : in out CPU_Context) is

      A_Native  : Native_Unsigned;
   begin
      A_Native := Native_Unsigned(CPU.Regs.A);

      if not Is_Set(CPU.Regs.F.N) then
         if Is_Set(CPU.Regs.F.H) or (A_Native and 16#0F#) > 9 then
            A_Native := A_Native + 16#06#;
         end if;
         if Is_Set(CPU.Regs.F.C) or A_Native > 16#9F# then
            A_Native := A_Native + 16#60#;
         end if;
      else
         if Is_Set(CPU.Regs.F.H) then
            A_Native := (A_Native - 6) and 16#FF#;
         end if;
         if Is_Set(CPU.Regs.F.C) then
            A_Native := A_Native - 16#60#;
         end if;
      end if;

      Reset(CPU.Regs.F.H);
      Set_Value(CPU.Regs.F.C, Is_Set(CPU.Regs.F.C) or (A_Native and 16#100#) /= 0);

      A_Native := A_Native and 16#FF#;

      Set_Value(CPU.Regs.F.Z, A_Native = 0);

      CPU.Regs.A := Byte(A_Native);
   end Do_Daa;

end Gade.Dev.CPU.Arithmetic;
