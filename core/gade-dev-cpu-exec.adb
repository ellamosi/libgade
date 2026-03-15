with Gade.Dev.CPU.Decode;
with Gade.Dev.CPU.Decoded_Instructions; use Gade.Dev.CPU.Decoded_Instructions;
with Gade.GB;                           use Gade.GB;

package body Gade.Dev.CPU.Exec is

   procedure Execute
     (CPU    : in out CPU_Context;
      GB     : in out Gade.GB.GB_Type;
      Cycles : out M_Cycle_Count) is
      pragma Unreferenced (CPU);
      Instruction : Gade.Dev.CPU.Decode.Decoded_Instruction;
   begin
      Instruction := Gade.Dev.CPU.Decode.Decode (GB);
      GB.CPU.PC := GB.CPU.PC + Word (Instruction.Length);

      GB.CPU.Branch_Taken := False;
      Gade.Dev.CPU.Decoded_Instructions.Execute (GB, Instruction);

      if GB.CPU.Branch_Taken then
         Cycles := Instruction.Jump_Cycles;
      else
         Cycles := Instruction.Cycles;
      end if;
   end Execute;

end Gade.Dev.CPU.Exec;
