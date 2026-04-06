with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;

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
      Pending_Enabled_Interrupt : constant Boolean := Has_Pending_Enabled_Interrupt (GB);
   begin
      if not Pending_Enabled_Interrupt then
         --  No enabled interrupt is pending, so HALT really enters the halted
         --  state and waits for one to arrive.
         GB.CPU.Execution_State := Halted;
      elsif Pending_Enabled_Interrupt and GB.CPU.IFF = IME_Enabled then
         --  An enabled interrupt is already pending and IME is on, so HALT is
         --  immediately broken by normal interrupt servicing.
         GB.CPU.Execution_State := Running;
      else
         --  An enabled interrupt is pending while IME is off, so HALT does not
         --  stick. Instead, the next instruction fetch reuses the current PC
         --  once before normal PC incrementing resumes.
         GB.CPU.Execution_State := Halt_Bug_Pending;
      end if;
   end HALT;

   procedure STOP (GB : in out GB_Type) is
   begin
      GB.CPU.PC := GB.CPU.PC + 1;
      if GB.CPU.IFF = IME_Enabled then
         GB.CPU.Execution_State := Halted;
      end if;
   end STOP;

   procedure DI (GB : in out GB_Type) is
   begin
      GB.CPU.IFF := IME_Disabled;
   end DI;

   procedure EI (GB : in out GB_Type) is
   begin
      GB.CPU.IFF := IME_Enable_Pending;
   end EI;

end Gade.Dev.CPU.Instructions.Control;
