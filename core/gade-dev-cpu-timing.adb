package body Gade.Dev.CPU.Timing is

   function Total_Cycles
     (Template     : Stage_Template;
      Branch_Taken : Boolean) return M_Cycle_Count
   is
      Total : M_Cycle_Count := 0;
   begin
      for Index in 1 .. Template.Count loop
         declare
            Item : constant Stage := Template.Items (Positive (Index));
         begin
            if Item.Flag /= Skip_If_Not_Taken or else Branch_Taken then
               Total := Total + Item.Cost;
            end if;
         end;
      end loop;

      return Total;
   end Total_Cycles;

end Gade.Dev.CPU.Timing;
