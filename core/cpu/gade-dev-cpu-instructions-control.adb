package body Gade.Dev.CPU.Instructions.Control is

   procedure Execute_NOP
     (GB : in out Gade.GB.GB_Type) is
      pragma Unreferenced (GB);
   begin
      null;
   end Execute_NOP;

   procedure Execute_CPL
     (GB : in out Gade.GB.GB_Type) is
   begin
      GB.CPU.Regs.A := not GB.CPU.Regs.A;
      Set (GB.CPU.Regs.F.H);
      Set (GB.CPU.Regs.F.N);
   end Execute_CPL;

   procedure Execute_SCF
     (GB : in out Gade.GB.GB_Type) is
   begin
      Reset (GB.CPU.Regs.F.N);
      Reset (GB.CPU.Regs.F.H);
      Set (GB.CPU.Regs.F.C);
   end Execute_SCF;

   procedure Execute_CCF
     (GB : in out Gade.GB.GB_Type) is
   begin
      Reset (GB.CPU.Regs.F.N);
      Reset (GB.CPU.Regs.F.H);
      Set_Value (GB.CPU.Regs.F.C, not Is_Set (GB.CPU.Regs.F.C));
   end Execute_CCF;

   procedure Execute_HALT
     (GB : in out Gade.GB.GB_Type) is
   begin
      GB.CPU.Halted := True;
   end Execute_HALT;

   procedure Execute_STOP
     (GB : in out Gade.GB.GB_Type) is
   begin
      GB.CPU.PC := GB.CPU.PC + 1;
      if GB.CPU.IFF = IE_EI then
         GB.CPU.Halted := True;
      end if;
   end Execute_STOP;

   procedure Execute_DI
     (GB : in out Gade.GB.GB_Type) is
   begin
      GB.CPU.IFF := IE_DI;
   end Execute_DI;

   procedure Execute_EI
     (GB : in out Gade.GB.GB_Type) is
   begin
      GB.CPU.IFF := IE_EI;
   end Execute_EI;

end Gade.Dev.CPU.Instructions.Control;
