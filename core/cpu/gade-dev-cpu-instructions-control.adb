package body Gade.Dev.CPU.Instructions.Control is

   procedure NOP (GB : in out GB_Type) is
      pragma Unreferenced (GB);
   begin
      null;
   end NOP;

   procedure CPL (GB : in out GB_Type) is
   begin
      GB.CPU.Regs.A := not GB.CPU.Regs.A;
      Set (GB.CPU.Regs.F.H);
      Set (GB.CPU.Regs.F.N);
   end CPL;

   procedure SCF (GB : in out GB_Type) is
   begin
      Reset (GB.CPU.Regs.F.N);
      Reset (GB.CPU.Regs.F.H);
      Set (GB.CPU.Regs.F.C);
   end SCF;

   procedure CCF (GB : in out GB_Type) is
   begin
      Reset (GB.CPU.Regs.F.N);
      Reset (GB.CPU.Regs.F.H);
      Set_Value (GB.CPU.Regs.F.C, not Is_Set (GB.CPU.Regs.F.C));
   end CCF;

   procedure HALT (GB : in out GB_Type) is
   begin
      GB.CPU.Halted := True;
   end HALT;

   procedure STOP (GB : in out GB_Type) is
   begin
      GB.CPU.PC := GB.CPU.PC + 1;
      if GB.CPU.IFF = IE_EI then
         GB.CPU.Halted := True;
      end if;
   end STOP;

   procedure DI (GB : in out GB_Type) is
   begin
      GB.CPU.IFF := IE_DI;
   end DI;

   procedure EI (GB : in out GB_Type) is
   begin
      GB.CPU.IFF := IE_EI;
   end EI;

end Gade.Dev.CPU.Instructions.Control;
