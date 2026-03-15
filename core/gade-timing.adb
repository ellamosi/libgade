package body Gade.Timing is

   function To_T_Cycles (M : M_Cycle_Count) return T_Cycle_Count is
   begin
      return T_Cycle_Count (M) * T_Cycles_Per_M_Cycle;
   end To_T_Cycles;

   function To_M_Cycles (T : T_Cycle_Count) return M_Cycle_Count is
   begin
      pragma Assert (T mod T_Cycles_Per_M_Cycle = 0);
      return M_Cycle_Count (T / T_Cycles_Per_M_Cycle);
   end To_M_Cycles;

end Gade.Timing;
