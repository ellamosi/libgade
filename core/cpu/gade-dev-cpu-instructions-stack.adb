package body Gade.Dev.CPU.Instructions.Stack is

   procedure Push
     (GB : in out Gade.GB.GB_Type) is
   begin
      Instructions.Internal_Cycle (GB);
      Instructions.Push_Word (GB, Instructions.Read_Word_Register (GB, Source));
   end Push;

   procedure Pop
     (GB : in out Gade.GB.GB_Type) is
      Value : Word;
   begin
      Instructions.Pop_Word (GB, Value);
      Instructions.Write_Word_Register (GB, Dest, Value);
   end Pop;

end Gade.Dev.CPU.Instructions.Stack;
