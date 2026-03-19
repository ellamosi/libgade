package Gade.Dev.CPU.Instructions.Stack is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Execute_PUSH_BC is new Instructions.Execute_Push
     (Source => Instructions.REG_BC);

   procedure Execute_PUSH_DE is new Instructions.Execute_Push
     (Source => Instructions.REG_DE);

   procedure Execute_PUSH_HL is new Instructions.Execute_Push
     (Source => Instructions.REG_HL);

   procedure Execute_PUSH_AF is new Instructions.Execute_Push
     (Source => Instructions.REG_AF);

   procedure Execute_POP_BC is new Instructions.Execute_Pop
     (Dest => Instructions.REG_BC);

   procedure Execute_POP_DE is new Instructions.Execute_Pop
     (Dest => Instructions.REG_DE);

   procedure Execute_POP_HL is new Instructions.Execute_Pop
     (Dest => Instructions.REG_HL);

   procedure Execute_POP_AF is new Instructions.Execute_Pop
     (Dest => Instructions.REG_AF);

end Gade.Dev.CPU.Instructions.Stack;
