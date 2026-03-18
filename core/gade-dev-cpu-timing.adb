with Gade.Dev.CPU.Staging; use Gade.Dev.CPU.Staging;

package body Gade.Dev.CPU.Timing is

   type Cycle_Table is array (Byte) of M_Cycle_Count;

   function Sum_Cycles
     (Template     : Stage_Template;
      Branch_Taken : Boolean) return M_Cycle_Count;

   function Build_Cycle_Table
     (Prefix       : Prefix_Kind;
      Branch_Taken : Boolean) return Cycle_Table;

   function Sum_Cycles
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
   end Sum_Cycles;

   function Build_Cycle_Table
     (Prefix       : Prefix_Kind;
      Branch_Taken : Boolean) return Cycle_Table
   is
      Result : Cycle_Table := [others => 0];
   begin
      for Opcode in Byte'Range loop
         Result (Opcode) :=
           Sum_Cycles
             (Template_For (Decode_Template (Prefix => Prefix, Opcode => Opcode)),
              Branch_Taken);
      end loop;

      return Result;
   end Build_Cycle_Table;

   Main_Cycle_Table         : constant Cycle_Table := Build_Cycle_Table (Main, False);
   Main_Taken_Cycle_Table   : constant Cycle_Table := Build_Cycle_Table (Main, True);
   CB_Cycle_Table           : constant Cycle_Table := Build_Cycle_Table (CB, False);
   CB_Taken_Cycle_Table     : constant Cycle_Table := Build_Cycle_Table (CB, True);

   function Total_Cycles
     (Prefix       : Prefix_Kind;
      Opcode       : Byte;
      Branch_Taken : Boolean) return M_Cycle_Count
   is
   begin
      case Prefix is
         when Main =>
            if Branch_Taken then
               return Main_Taken_Cycle_Table (Opcode);
            else
               return Main_Cycle_Table (Opcode);
            end if;
         when CB =>
            if Branch_Taken then
               return CB_Taken_Cycle_Table (Opcode);
            else
               return CB_Cycle_Table (Opcode);
            end if;
      end case;
   end Total_Cycles;

end Gade.Dev.CPU.Timing;
