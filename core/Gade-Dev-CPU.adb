with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Dev.CPU is

   procedure Reset (ctxt : in out CPU_Context) is
   begin
      ctxt.Regs.AF  := 16#0000#;
      ctxt.Regs.BC  := 16#0000#;
      ctxt.Regs.DE  := 16#0000#;
      ctxt.Regs.HL  := 16#0000#;
      ctxt.Regs.SP  := 16#FFFE#;
      ctxt.PC       := 16#0100#;
      ctxt.Regs.F.Z := True;
   end Reset;

   procedure Val_Flag (ctx : in out CPU_Context) is
   begin
      null;
   end Val_Flag;

   procedure Set (Flag : in out CPU_Flag) is
   begin
      Flag := True;
   end Set;

   procedure Set_Value (Flag : in out CPU_Flag; Value : in Boolean) is
   begin
      Flag := CPU_Flag(Value);
   end Set_Value;

   procedure Reset (Flag : in out CPU_Flag) is
   begin
      Flag := False;
   end Reset;

   function Is_Set (Flag : CPU_Flag) return Boolean is
   begin
      return Boolean(Flag);
   end Is_Set;

   function Check_Condition
      (CPU  : CPU_Context;
       cond : Condition_Type) return Boolean is
   begin
      case cond is
	when C_Z  => return Is_Set(CPU.Regs.F.Z);
	when C_NZ => return not Is_Set(CPU.Regs.F.Z);
	when C_C  => return Is_Set(CPU.Regs.F.C);
	when C_NC => return not Is_Set(CPU.Regs.F.C);
      end case;
   end Check_Condition;

   function Get_Flags_String (CPU : CPU_Context) return String is
      l : Natural;
      R : String (1..7);
   begin
      l := 0;
      if Is_Set(CPU.Regs.F.C) then
         l := l + 1;
         R(l) := 'C';
      end if;
      if Is_Set(CPU.Regs.F.N) then
         l := l + 1;
         R(l) := 'N';
      end if;
      if Is_Set(CPU.Regs.F.H) then
         l := l + 1;
         R(l) := 'H';
      end if;
      if Is_Set(CPU.Regs.F.Z) then
         l := l + 1;
         R(l) := 'Z';
      end if;
      return R(1..l);
   end Get_Flags_String;

end Gade.Dev.CPU;
