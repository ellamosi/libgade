with Gade.Dev.CPU.Decode; use Gade.Dev.CPU.Decode;
with Gade.Timing;

package Gade.Dev.CPU.Staging is

   --  Fixed-size stage templates avoid heap allocation while keeping the
   --  instruction timing model structural and inspectable.

   type Stage_Kind is
     (Fetch_Imm8,     --  Fetch an 8-bit immediate operand
      Fetch_Imm16_Lo, --  Fetch low byte of a 16-bit immediate
      Fetch_Imm16_Hi, --  Fetch high byte of a 16-bit immediate
      Read,           --  Read from a resolved memory operand
      Write,          --  Write to a resolved memory operand
      Internal,       --  Internal CPU-only work with no bus transfer
      Push_Hi,        --  Push high byte onto the stack
      Push_Lo,        --  Push low byte onto the stack
      Pop_Lo,         --  Pop low byte from the stack
      Pop_Hi,         --  Pop high byte from the stack
      Cond_Check,     --  Evaluate a branch condition
      Commit);        --  Commit the instruction result or control transfer

   type Stage_Flag is
     (None,             --  Stage always contributes to execution/timing
      Skip_If_Not_Taken --  Stage only applies when a conditional branch is taken
     );

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
