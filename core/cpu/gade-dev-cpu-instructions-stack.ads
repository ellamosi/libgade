package Gade.Dev.CPU.Instructions.Stack is
   package Instructions renames Gade.Dev.CPU.Instructions;

   generic
      Source : Instructions.Word_Register_Kind;
   procedure Execute_Push
     (GB : in out Gade.GB.GB_Type);

   generic
      Dest : Instructions.Word_Register_Kind;
   procedure Execute_Pop
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions.Stack;
