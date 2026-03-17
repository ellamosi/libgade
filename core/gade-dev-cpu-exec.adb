with Gade.Dev.CPU.Decode;
with Gade.Dev.CPU.Decoded_Instructions; use Gade.Dev.CPU.Decoded_Instructions;
with Gade.Dev.CPU.Generic_Dispatch_Prototype;
with Gade.Dev.CPU.Staging;
with Gade.Dev.CPU.Timing;
with Gade.GB;                           use Gade.GB;

package body Gade.Dev.CPU.Exec is

   use type Gade.Dev.CPU.Generic_Dispatch_Prototype.Instruction_Handler;

   procedure Execute
     (CPU    : in out CPU_Context;
      GB     : in out Gade.GB.GB_Type;
      Cycles : out M_Cycle_Count) is
      pragma Unreferenced (CPU);
      Instruction : Gade.Dev.CPU.Decode.Decoded_Instruction;
      Handler     : Gade.Dev.CPU.Generic_Dispatch_Prototype.Instruction_Handler;
      Template    : Gade.Dev.CPU.Staging.Stage_Template;
   begin
      Instruction := Gade.Dev.CPU.Decode.Decode (GB);
      Template := Gade.Dev.CPU.Staging.Template_For (Instruction);

      GB.CPU.Branch_Taken := False;
      case Instruction.Prefix is
         when Gade.Dev.CPU.Decode.Main =>
            Handler :=
              Gade.Dev.CPU.Generic_Dispatch_Prototype.Main_Handler
                (Instruction.Opcode);
            if Handler /= null then
               GB.CPU.PC := GB.CPU.PC + 1;
               Handler.all (GB);
            else
               GB.CPU.PC := GB.CPU.PC + Word (Instruction.Length);
               Gade.Dev.CPU.Decoded_Instructions.Execute (GB, Instruction);
            end if;

         when Gade.Dev.CPU.Decode.CB =>
            Handler :=
              Gade.Dev.CPU.Generic_Dispatch_Prototype.CB_Handler
                (Instruction.Opcode);
            if Handler /= null then
               GB.CPU.PC := GB.CPU.PC + 2;
               Handler.all (GB);
            else
               GB.CPU.PC := GB.CPU.PC + Word (Instruction.Length);
               Gade.Dev.CPU.Decoded_Instructions.Execute (GB, Instruction);
            end if;
      end case;

      Cycles := Gade.Dev.CPU.Timing.Total_Cycles (Template, GB.CPU.Branch_Taken);
   end Execute;

end Gade.Dev.CPU.Exec;
