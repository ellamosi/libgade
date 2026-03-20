package Gade.Dev.CPU.Instructions.Stack.Handlers is

   --  PUSH
   procedure PUSH_AF is new Push (REG_AF);
   procedure PUSH_BC is new Push (REG_BC);
   procedure PUSH_DE is new Push (REG_DE);
   procedure PUSH_HL is new Push (REG_HL);

   --  POP
   procedure POP_AF is new Pop (REG_AF);
   procedure POP_BC is new Pop (REG_BC);
   procedure POP_DE is new Pop (REG_DE);
   procedure POP_HL is new Pop (REG_HL);

end Gade.Dev.CPU.Instructions.Stack.Handlers;
