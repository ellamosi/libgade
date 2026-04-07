package body Gade.Dev.CPU.Instructions.Logic is

   procedure Adjust_Logic_Flags (CPU : in out CPU_Context; Set_H : Boolean) is
   begin
      Set_Value (CPU.Regs.F.Z, CPU.Regs.A = 0);
      Reset (CPU.Regs.F.N);
      Set_Value (CPU.Regs.F.H, Set_H);
      Reset (CPU.Regs.F.C);
   end Adjust_Logic_Flags;

   procedure Logic_AND (CPU : in out CPU_Context; Value : Byte) is
   begin
      CPU.Regs.A := CPU.Regs.A and Value;
      Adjust_Logic_Flags (CPU, True);
   end Logic_AND;

   procedure Logic_OR (CPU : in out CPU_Context; Value : Byte) is
   begin
      CPU.Regs.A := CPU.Regs.A or Value;
      Adjust_Logic_Flags (CPU, False);
   end Logic_OR;

   procedure Logic_XOR (CPU : in out CPU_Context; Value : Byte) is
   begin
      CPU.Regs.A := CPU.Regs.A xor Value;
      Adjust_Logic_Flags (CPU, False);
   end Logic_XOR;

end Gade.Dev.CPU.Instructions.Logic;
