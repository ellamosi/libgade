with Gade.Dev.CPU.Generic_Dispatch_Prototype;
with Gade.Dev.CPU.Cycle_Steps;
with Gade.GB;                           use Gade.GB;

package body Gade.Dev.CPU.Exec is

   procedure Execute
     (CPU    : in out CPU_Context;
      GB     : in out Gade.GB.GB_Type;
      Cycles : out M_Cycle_Count) is
      pragma Unreferenced (CPU);
   begin
      Gade.Dev.CPU.Cycle_Steps.Reset (GB.CPU);
      Gade.Dev.CPU.Generic_Dispatch_Prototype.Execute (GB);
      Cycles := Gade.Dev.CPU.Cycle_Steps.Consumed_Cycles (GB.CPU);
   end Execute;

end Gade.Dev.CPU.Exec;
