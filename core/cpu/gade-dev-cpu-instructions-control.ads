with Gade.GB;

package Gade.Dev.CPU.Instructions.Control is
   procedure Execute_NOP
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_CPL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_SCF
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_CCF
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_HALT
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_STOP
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_DI
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_EI
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions.Control;
