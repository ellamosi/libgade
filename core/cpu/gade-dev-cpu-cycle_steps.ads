package Gade.Dev.CPU.Cycle_Steps is

   procedure Reset
     (CPU : in out CPU_Context);
   pragma Inline (Reset);

   procedure Step_M_Cycle
     (CPU    : in out CPU_Context;
      Cycles :        M_Cycle_Count := 1);
   pragma Inline (Step_M_Cycle);

   function Consumed_Cycles
     (CPU : CPU_Context) return M_Cycle_Count;
   pragma Inline (Consumed_Cycles);

end Gade.Dev.CPU.Cycle_Steps;
