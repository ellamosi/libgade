with Gade.Dev.CPU.Decode; use Gade.Dev.CPU.Decode;
with Gade.Timing;

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
      Cost : Gade.Timing.M_Cycle_Count := 1;
      Flag : Stage_Flag := None;
   end record;

   Max_Stage_Count : constant := 7;

   subtype Stage_Index is Positive range 1 .. Max_Stage_Count;
   type Stage_Count is range 0 .. Max_Stage_Count;
   type Stage_Array is array (Stage_Index) of Stage;

   type Stage_Template is record
      Count : Stage_Count := 0;
      Items : Stage_Array := [others => (others => <>)];
   end record;

   Empty_Template : constant Stage_Template :=
     (Count => 0, Items => [others => (others => <>)]);

   function Template_For
     (Inst : Decoded_Instruction) return Stage_Template;

end Gade.Dev.CPU.Staging;
