with Gade.GB;

package Gade.Dev.CPU.Instructions.Control is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Execute_NOP
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_NOP;

   procedure Execute_DAA
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_DAA;

   procedure Execute_CPL
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_CPL;

   procedure Execute_SCF
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_SCF;

   procedure Execute_CCF
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_CCF;

   procedure Execute_HALT
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_HALT;

   procedure Execute_STOP
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_STOP;

   procedure Execute_DI
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_DI;

   procedure Execute_EI
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_EI;

end Gade.Dev.CPU.Instructions.Control;
