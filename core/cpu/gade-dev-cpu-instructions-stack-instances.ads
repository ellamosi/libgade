package Gade.Dev.CPU.Instructions.Stack.Instances is
   procedure PUSH_BC is new Push
     (Source => REG_BC);

   procedure PUSH_DE is new Push
     (Source => REG_DE);

   procedure PUSH_HL is new Push
     (Source => REG_HL);

   procedure PUSH_AF is new Push
     (Source => REG_AF);

   procedure POP_BC is new Pop
     (Dest => REG_BC);

   procedure POP_DE is new Pop
     (Dest => REG_DE);

   procedure POP_HL is new Pop
     (Dest => REG_HL);

   procedure POP_AF is new Pop
     (Dest => REG_AF);

end Gade.Dev.CPU.Instructions.Stack.Instances;
