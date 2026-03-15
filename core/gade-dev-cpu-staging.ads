with Gade.Timing; use Gade.Timing;

package Gade.Dev.CPU.Staging is

   --  Fixed-size stage templates avoid heap allocation while keeping the
   --  instruction timing model structural and inspectable.

   type Stage_Kind is
     (Fetch_Imm8,
      Fetch_Imm16_Lo,
      Fetch_Imm16_Hi,
      Read,
      Write,
      Internal,
      Push_Hi,
      Push_Lo,
      Pop_Lo,
      Pop_Hi,
      Cond_Check,
      Commit);

   type Stage_Flag is (None, Skip_If_Not_Taken);

   type Stage is record
      Kind : Stage_Kind := Commit;
      Cost : M_Cycle_Count := 1;
      Flag : Stage_Flag := None;
   end record;

   Max_Stage_Count : constant := 6;

   subtype Stage_Index is Positive range 1 .. Max_Stage_Count;
   type Stage_Count is range 0 .. Max_Stage_Count;
   type Stage_Array is array (Stage_Index) of Stage;

   type Stage_Template is record
      Count : Stage_Count := 0;
      Items : Stage_Array := [others => (others => <>)];
   end record;

   Empty_Template : constant Stage_Template :=
     (Count => 0, Items => [others => (others => <>)]);

end Gade.Dev.CPU.Staging;
