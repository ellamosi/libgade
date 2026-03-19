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

   procedure Execute_INC_B_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_B);

   procedure Execute_DEC_B_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_B);

   procedure Execute_INC_C_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_C);

   procedure Execute_DEC_C_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_C);

   procedure Execute_INC_D_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_D);

   procedure Execute_DEC_D_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_D);

   procedure Execute_INC_E_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_E);

   procedure Execute_DEC_E_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_E);

   procedure Execute_INC_H_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_H);

   procedure Execute_DEC_H_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_H);

   procedure Execute_INC_L_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_L);

   procedure Execute_DEC_L_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_L);

   procedure Execute_INC_Addr_HL_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_DEC_Addr_HL_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_INC_A_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_A);

   procedure Execute_DEC_A_Impl is new Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_A);

   procedure Execute_INC_BC_Impl is new Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_BC);

   procedure Execute_DEC_BC_Impl is new Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_BC);

   procedure Execute_INC_DE_Impl is new Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_DE);

   procedure Execute_DEC_DE_Impl is new Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_DE);

   procedure Execute_INC_SP_Impl is new Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_SP);

   procedure Execute_INC_HL_Impl is new Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_HL);

   procedure Execute_DEC_HL_Impl is new Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_HL);

   procedure Execute_DEC_SP_Impl is new Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_SP);

   procedure Execute_ADD_HL_BC_Impl is new Execute_Add_HL
     (Source => Instructions.REG_BC);

   procedure Execute_ADD_HL_DE_Impl is new Execute_Add_HL
     (Source => Instructions.REG_DE);

   procedure Execute_ADD_HL_HL_Impl is new Execute_Add_HL
     (Source => Instructions.REG_HL);

   procedure Execute_ADD_HL_SP_Impl is new Execute_Add_HL
     (Source => Instructions.REG_SP);

   procedure Execute_INC_B
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_B_Impl;
   procedure Execute_DEC_B
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_B_Impl;
   procedure Execute_INC_C
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_C_Impl;
   procedure Execute_DEC_C
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_C_Impl;
   procedure Execute_INC_D
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_D_Impl;
   procedure Execute_DEC_D
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_D_Impl;
   procedure Execute_INC_E
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_E_Impl;
   procedure Execute_DEC_E
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_E_Impl;
   procedure Execute_INC_H
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_H_Impl;
   procedure Execute_DEC_H
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_H_Impl;
   procedure Execute_INC_L
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_L_Impl;
   procedure Execute_DEC_L
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_L_Impl;
   procedure Execute_INC_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_Addr_HL_Impl;
   procedure Execute_DEC_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_Addr_HL_Impl;
   procedure Execute_INC_A
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_A_Impl;
   procedure Execute_DEC_A
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_A_Impl;
   procedure Execute_INC_BC
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_BC_Impl;
   procedure Execute_DEC_BC
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_BC_Impl;
   procedure Execute_INC_DE
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_DE_Impl;
   procedure Execute_DEC_DE
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_DE_Impl;
   procedure Execute_INC_SP
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_SP_Impl;
   procedure Execute_INC_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_INC_HL_Impl;
   procedure Execute_DEC_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_HL_Impl;
   procedure Execute_DEC_SP
     (GB : in out Gade.GB.GB_Type) renames Execute_DEC_SP_Impl;
   procedure Execute_ADD_HL_BC
     (GB : in out Gade.GB.GB_Type) renames Execute_ADD_HL_BC_Impl;
   procedure Execute_ADD_HL_DE
     (GB : in out Gade.GB.GB_Type) renames Execute_ADD_HL_DE_Impl;
   procedure Execute_ADD_HL_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_ADD_HL_HL_Impl;
   procedure Execute_ADD_HL_SP
     (GB : in out Gade.GB.GB_Type) renames Execute_ADD_HL_SP_Impl;

end Gade.Dev.CPU.Instructions.Arithmetic;
