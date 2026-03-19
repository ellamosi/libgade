package body Gade.Dev.CPU.Instructions.Arithmetic is

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
      A_Native := Native_Unsigned (CPU.Regs.A);
      Value_Native := Native_Unsigned (Value);

      Reset (CPU.Regs.F.N);

      if Boolean (Carry) and Is_Set (CPU.Regs.F.C) then
         Carry_Value := 1;
      else
         Carry_Value := 0;
      end if;

      Set_Value
        (CPU.Regs.F.H,
         (((A_Native and 16#0F#) + (Value_Native and 16#0F#) + Carry_Value)
          and 16#10#) /= 0);
      Result_Native := A_Native + Value_Native + Carry_Value;
      Result := Byte (Result_Native and 16#FF#);
      Set_Value (CPU.Regs.F.Z, Result = 0);
      Set_Value (CPU.Regs.F.C, (Result_Native and 16#100#) /= 0);
   end Do_Add;

   procedure Do_Add
     (CPU   : in out CPU_Context;
      Reg   : in out Word;
      Value : Byte) is
      Reg_Native    : Native_Unsigned;
      Value_Native  : Native_Unsigned;
      Result_Native : Native_Unsigned;
   begin
      Reg_Native := Native_Unsigned (Reg);
      Value_Native := Native_Unsigned (Value);

      Reset (CPU.Regs.F.N);
      Reset (CPU.Regs.F.Z);

      if (Value_Native and 16#80#) /= 0 then
         Value_Native := Value_Native or 16#FF00#;
      end if;

      Set_Value
        (CPU.Regs.F.H,
         (((Reg_Native and 16#000F#) + (Value_Native and 16#0F#))
          and 16#10#) /= 0);
      Result_Native := Reg_Native + Value_Native;
      Set_Value
        (CPU.Regs.F.C,
         (((Reg_Native and 16#00FF#) + (Value_Native and 16#00FF#))
          and 16#100#) /= 0);

      Reg := Word (Result_Native and 16#FFFF#);
   end Do_Add;

   procedure Do_Add
     (CPU    : in out CPU_Context;
      Value  : Word;
      Result : out Word) is

      HL_Native     : Native_Unsigned;
      Value_Native  : Native_Unsigned;
      Result_Native : Native_Unsigned;
   begin
      HL_Native := Native_Unsigned (CPU.Regs.HL);
      Value_Native := Native_Unsigned (Value);

      Reset (CPU.Regs.F.N);

      Set_Value
        (CPU.Regs.F.H,
         (((HL_Native and 16#0FFF#) + (Value_Native and 16#0FFF#))
          and 16#1000#) /= 0);
      Result_Native := HL_Native + Value_Native;

      Result := Word (Result_Native and 16#FFFF#);
      Set_Value (CPU.Regs.F.C, (Result_Native and 16#10000#) /= 0);
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
      A_Native := Native_Unsigned (CPU.Regs.A);
      Value_Native := Native_Unsigned (Value);

      Set (CPU.Regs.F.N);

      if Boolean (Carry) and Is_Set (CPU.Regs.F.C) then
         Carry_Value := 1;
      else
         Carry_Value := 0;
      end if;

      Set_Value
        (CPU.Regs.F.H,
         (((A_Native and 16#0F#) - (Value_Native and 16#0F#) - Carry_Value)
          and 16#10#) /= 0);
      Result_Native := A_Native - Value_Native - Carry_Value;

      Result := Byte (Result_Native and 16#FF#);
      Set_Value (CPU.Regs.F.Z, Result = 0);
      Set_Value (CPU.Regs.F.C, (Result_Native and 16#100#) /= 0);
   end Do_Sub;

   procedure Add_Offset
     (CPU       : in out CPU_Context;
      Address   : in out Word;
      Offset    :        Byte;
      Set_Flags :        Boolean) is
      Result_Native  : Native_Unsigned;
      Address_Native : Native_Unsigned;
      Offset_Native  : Native_Unsigned;
   begin
      Address_Native := Native_Unsigned (Address);
      Offset_Native := Native_Unsigned (Offset);
      if (Offset_Native and 16#80#) = 0 then
         Result_Native := Address_Native + Offset_Native;
         if Set_Flags then
            Set_Value (CPU.Regs.F.C, (Result_Native and 16#10000#) /= 0);
         end if;
      else
         Result_Native := Address_Native - ((not Offset_Native) and 16#7F#) - 1;
         if Set_Flags then
            Set_Value (CPU.Regs.F.C, (Result_Native and 16#10000#) /= 0);
         end if;
      end if;
      Address := Word (Result_Native and 16#FFFF#);
   end Add_Offset;

   procedure Do_Inc_Dec
     (CPU     : in out CPU_Context;
      Inc_Dec :        Inc_Dec_Type;
      Value   :        Byte;
      Result  :    out Byte) is
   begin
      if Inc_Dec = DEC then
         Result := Value - 1;
         Set_Value (CPU.Regs.F.H, (Value and 16#0F#) = 16#0#);
      else
         Result := Value + 1;
         Set_Value (CPU.Regs.F.H, (Value and 16#0F#) = 16#F#);
      end if;

      Set_Value (CPU.Regs.F.Z, Result = 0);
      Set_Value (CPU.Regs.F.N, Inc_Dec = DEC);
   end Do_Inc_Dec;

   procedure Do_Daa
     (CPU : in out CPU_Context) is
      A_Native : Native_Unsigned;
   begin
      A_Native := Native_Unsigned (CPU.Regs.A);

      if not Is_Set (CPU.Regs.F.N) then
         if Is_Set (CPU.Regs.F.H) or (A_Native and 16#0F#) > 9 then
            A_Native := A_Native + 16#06#;
         end if;
         if Is_Set (CPU.Regs.F.C) or A_Native > 16#9F# then
            A_Native := A_Native + 16#60#;
         end if;
      else
         if Is_Set (CPU.Regs.F.H) then
            A_Native := (A_Native - 6) and 16#FF#;
         end if;
         if Is_Set (CPU.Regs.F.C) then
            A_Native := A_Native - 16#60#;
         end if;
      end if;

      Reset (CPU.Regs.F.H);
      Set_Value (CPU.Regs.F.C, Is_Set (CPU.Regs.F.C) or (A_Native and 16#100#) /= 0);

      A_Native := A_Native and 16#FF#;
      Set_Value (CPU.Regs.F.Z, A_Native = 0);
      CPU.Regs.A := Byte (A_Native);
   end Do_Daa;

   procedure Execute_ADD_SP_Imm8
     (GB : in out Gade.GB.GB_Type) is
   begin
      Do_Add (GB.CPU, GB.CPU.Regs.SP, Instructions.Fetch_Imm8 (GB));
      Instructions.Internal_Cycle (GB);
      Instructions.Internal_Cycle (GB);
   end Execute_ADD_SP_Imm8;

   procedure Execute_DAA
     (GB : in out Gade.GB.GB_Type) is
   begin
      Do_Daa (GB.CPU);
   end Execute_DAA;

   procedure Execute_Inc_Dec_Byte
     (GB : in out Gade.GB.GB_Type) is
      Value : Byte := Instructions.Load_Target (GB, Target);
   begin
      Do_Inc_Dec
        (GB.CPU,
         (if Operation = Instructions.OP_INC then INC else DEC),
         Value,
         Value);
      Instructions.Store_Target (GB, Target, Value);
   end Execute_Inc_Dec_Byte;

   procedure Execute_Inc_Dec_Word
     (GB : in out Gade.GB.GB_Type) is
      Value : Word := Instructions.Read_Word_Register (GB, Target);
   begin
      if Operation = Instructions.OP_INC then
         Value := Value + 1;
      else
         Value := Value - 1;
      end if;
      Instructions.Internal_Cycle (GB);
      Instructions.Write_Word_Register (GB, Target, Value);
   end Execute_Inc_Dec_Word;

   procedure Execute_Add_HL
     (GB : in out Gade.GB.GB_Type) is
      Value : Word := GB.CPU.Regs.HL;
   begin
      Do_Add (GB.CPU, Instructions.Read_Word_Register (GB, Source), Value);
      Instructions.Internal_Cycle (GB);
      GB.CPU.Regs.HL := Value;
   end Execute_Add_HL;

end Gade.Dev.CPU.Instructions.Arithmetic;
