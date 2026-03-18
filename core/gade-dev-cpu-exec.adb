with Gade.Dev.CPU.Generic_Dispatch_Prototype;
with Gade.Dev.CPU.Cycle_Steps;
with Gade.GB;                           use Gade.GB;

package body Gade.Dev.CPU.Exec is

   procedure Execute
     (CPU    : in out CPU_Context;
      GB     : in out Gade.GB.GB_Type;
      Cycles : out M_Cycle_Count) is
      pragma Unreferenced (CPU);
      Expected_Cycles : M_Cycle_Count;
   begin
      Gade.Dev.CPU.Cycle_Steps.Reset (GB.CPU);
      Gade.Dev.CPU.Generic_Dispatch_Prototype.Execute (GB, Expected_Cycles);
      Cycles := Gade.Dev.CPU.Cycle_Steps.Consumed_Cycles (GB.CPU);

      if Cycles /= Expected_Cycles then
         raise Program_Error with
           "cycle mismatch: stepped="
           & M_Cycle_Count'Image (Cycles)
           & " expected="
           & M_Cycle_Count'Image (Expected_Cycles);
      end if;
   end Execute;

end Gade.Dev.CPU.Exec;
