package Gade.Dev.CPU.Instructions.Stack.Instances is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure PUSH_BC is new Gade.Dev.CPU.Instructions.Stack.Push
     (Source => Instructions.REG_BC);

   procedure PUSH_DE is new Gade.Dev.CPU.Instructions.Stack.Push
     (Source => Instructions.REG_DE);

   procedure PUSH_HL is new Gade.Dev.CPU.Instructions.Stack.Push
     (Source => Instructions.REG_HL);

   procedure PUSH_AF is new Gade.Dev.CPU.Instructions.Stack.Push
     (Source => Instructions.REG_AF);

   procedure POP_BC is new Gade.Dev.CPU.Instructions.Stack.Pop
     (Dest => Instructions.REG_BC);

   procedure POP_DE is new Gade.Dev.CPU.Instructions.Stack.Pop
     (Dest => Instructions.REG_DE);

   procedure POP_HL is new Gade.Dev.CPU.Instructions.Stack.Pop
     (Dest => Instructions.REG_HL);

   procedure POP_AF is new Gade.Dev.CPU.Instructions.Stack.Pop
     (Dest => Instructions.REG_AF);

end Gade.Dev.CPU.Instructions.Stack.Instances;
