with Gade.GB;

package Gade.Dev.CPU.Dispatch is

   type Instruction_Handler is access procedure (GB : in out Gade.GB.GB_Type);

   procedure Execute
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Dispatch;
