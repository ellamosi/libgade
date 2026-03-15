package Gade.Timing is

   type T_Cycle_Count is new Natural;
   type M_Cycle_Count is new Natural;

   T_Cycles_Per_M_Cycle : constant := 4;

   function To_T_Cycles (M : M_Cycle_Count) return T_Cycle_Count;
   function To_M_Cycles (T : T_Cycle_Count) return M_Cycle_Count;

end Gade.Timing;
