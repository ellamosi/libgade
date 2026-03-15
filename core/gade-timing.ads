package Gade.Timing is

   --  Scheduling boundary rules:
   --  - Cross-package execution/reporting APIs use M_Cycle_Count.
   --  - Subsystems that still model hardware in raw clock edges keep
   --    T_Cycle_Count internally and convert explicitly at their boundary.
   --  - Do not mix the two units without calling To_T_Cycles/To_M_Cycles.

   type T_Cycle_Count is new Natural;
   type M_Cycle_Count is new Natural;

   T_Cycles_Per_M_Cycle : constant := 4;

   function To_T_Cycles (M : M_Cycle_Count) return T_Cycle_Count;
   function To_M_Cycles (T : T_Cycle_Count) return M_Cycle_Count;

end Gade.Timing;
