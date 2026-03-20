package Gade.Dev.CPU.Instructions.Logic is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Logic_AND
     (CPU   : in out CPU_Context;
      Value :        Byte);

   procedure Logic_OR
     (CPU   : in out CPU_Context;
      Value :        Byte);

   procedure Logic_XOR
     (CPU   : in out CPU_Context;
      Value :        Byte);

private

   procedure Adjust_Logic_Flags
     (CPU   : in out CPU_Context;
      Set_H :        Boolean);

end Gade.Dev.CPU.Instructions.Logic;
