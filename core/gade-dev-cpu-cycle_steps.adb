package body Gade.Dev.CPU.Cycle_Steps is

   procedure Reset
     (CPU : in out CPU_Context) is
   begin
      CPU.Stepped_Cycles := 0;
   end Reset;

   procedure Step_M_Cycle
     (CPU    : in out CPU_Context;
      Cycles :        M_Cycle_Count := 1) is
   begin
      CPU.Stepped_Cycles := CPU.Stepped_Cycles + Cycles;
   end Step_M_Cycle;

   function Consumed_Cycles
     (CPU : CPU_Context) return M_Cycle_Count is
   begin
      return CPU.Stepped_Cycles;
   end Consumed_Cycles;

end Gade.Dev.CPU.Cycle_Steps;
