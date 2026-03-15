with Gade.Dev.CPU.Staging; use Gade.Dev.CPU.Staging;

package Gade.Dev.CPU.Timing is

   function Total_Cycles
     (Template     : Stage_Template;
      Branch_Taken : Boolean) return M_Cycle_Count;

end Gade.Dev.CPU.Timing;
