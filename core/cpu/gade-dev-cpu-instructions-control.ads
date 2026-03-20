with Gade.GB;

package Gade.Dev.CPU.Instructions.Control is
   procedure NOP
     (GB : in out Gade.GB.GB_Type);

   procedure CPL
     (GB : in out Gade.GB.GB_Type);

   procedure SCF
     (GB : in out Gade.GB.GB_Type);

   procedure CCF
     (GB : in out Gade.GB.GB_Type);

   procedure HALT
     (GB : in out Gade.GB.GB_Type);

   procedure STOP
     (GB : in out Gade.GB.GB_Type);

   procedure DI
     (GB : in out Gade.GB.GB_Type);

   procedure EI
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions.Control;
