package Gade.Dev.CPU.Logic is

   procedure Do_AND
     (CPU     : in out CPU_Context;
      Value   : Byte);

   procedure Do_OR
     (CPU     : in out CPU_Context;
      Value   : Byte);

   procedure Do_XOR
     (CPU     : in out CPU_Context;
      Value   : Byte);

private

   procedure Adjust_Logic_Flags
     (CPU     : in out CPU_Context;
      Set_H   : Boolean);

end Gade.Dev.CPU.Logic;
