limited with Gade.GB;

package Gade.Dev.CPU.Instructions.Exec is

   procedure Execute
     (CPU    : in out CPU_Context;
      GB     : in out Gade.GB.GB_Type;
      Cycles : out Natural);

end Gade.Dev.CPU.Instructions.Exec;
