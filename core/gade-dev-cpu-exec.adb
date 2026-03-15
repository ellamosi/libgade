with Gade.Dev.CPU.Decode;
with Gade.Dev.CPU.Decoded_Instructions; use Gade.Dev.CPU.Decoded_Instructions;
with Gade.Dev.CPU.Staging;
with Gade.Dev.CPU.Timing;
with Gade.GB;                           use Gade.GB;

package body Gade.Dev.CPU.Exec is

   procedure Execute
     (CPU    : in out CPU_Context;
      GB     : in out Gade.GB.GB_Type;
      Cycles : out M_Cycle_Count) is
      pragma Unreferenced (CPU);
      Instruction : Gade.Dev.CPU.Decode.Decoded_Instruction;
      Template    : Gade.Dev.CPU.Staging.Stage_Template;
   begin
      Instruction := Gade.Dev.CPU.Decode.Decode (GB);
      Template := Gade.Dev.CPU.Staging.Template_For (Instruction);
      GB.CPU.PC := GB.CPU.PC + Word (Instruction.Length);

      GB.CPU.Branch_Taken := False;
      Gade.Dev.CPU.Decoded_Instructions.Execute (GB, Instruction);
      Cycles := Gade.Dev.CPU.Timing.Total_Cycles (Template, GB.CPU.Branch_Taken);
   end Execute;

end Gade.Dev.CPU.Exec;
