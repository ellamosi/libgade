with Gade.Dev.CPU.Decode;
with Gade.GB;

package Gade.Dev.CPU.Generic_Dispatch_Prototype is

   type Instruction_Handler is access procedure (GB : in out Gade.GB.GB_Type);

   function Main_Handler
     (Opcode : Byte) return Instruction_Handler;

   function CB_Handler
     (Opcode : Byte) return Instruction_Handler;

   procedure Execute
     (GB          : in out Gade.GB.GB_Type;
      Instruction :        Gade.Dev.CPU.Decode.Decoded_Instruction);

end Gade.Dev.CPU.Generic_Dispatch_Prototype;
