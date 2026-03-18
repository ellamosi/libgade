with Gade.GB;
with Gade.Timing;

package Gade.Dev.CPU.Generic_Dispatch_Prototype is

   type Instruction_Handler is access procedure (GB : in out Gade.GB.GB_Type);

   function Main_Handler
     (Opcode : Byte) return Instruction_Handler;

   function CB_Handler
     (Opcode : Byte) return Instruction_Handler;

   procedure Execute
     (GB     : in out Gade.GB.GB_Type;
      Cycles :    out Gade.Timing.M_Cycle_Count);

end Gade.Dev.CPU.Generic_Dispatch_Prototype;
