package body Gade.Dev.CPU.Instructions.Stack is

   procedure Execute_Push
     (GB : in out Gade.GB.GB_Type) is
   begin
      Instructions.Internal_Cycle (GB);
      Instructions.Push_Word (GB, Instructions.Read_Word_Register (GB, Source));
   end Execute_Push;

   procedure Execute_Pop
     (GB : in out Gade.GB.GB_Type) is
      Value : Word;
   begin
      Instructions.Pop_Word (GB, Value);
      Instructions.Write_Word_Register (GB, Dest, Value);
   end Execute_Pop;

   procedure Execute_PUSH_BC_Impl is new Execute_Push
     (Source => Instructions.REG_BC);

   procedure Execute_PUSH_DE_Impl is new Execute_Push
     (Source => Instructions.REG_DE);

   procedure Execute_PUSH_HL_Impl is new Execute_Push
     (Source => Instructions.REG_HL);

   procedure Execute_PUSH_AF_Impl is new Execute_Push
     (Source => Instructions.REG_AF);

   procedure Execute_POP_BC_Impl is new Execute_Pop
     (Dest => Instructions.REG_BC);

   procedure Execute_POP_DE_Impl is new Execute_Pop
     (Dest => Instructions.REG_DE);

   procedure Execute_POP_HL_Impl is new Execute_Pop
     (Dest => Instructions.REG_HL);

   procedure Execute_POP_AF_Impl is new Execute_Pop
     (Dest => Instructions.REG_AF);

   procedure Execute_PUSH_BC
     (GB : in out Gade.GB.GB_Type) renames Execute_PUSH_BC_Impl;

   procedure Execute_PUSH_DE
     (GB : in out Gade.GB.GB_Type) renames Execute_PUSH_DE_Impl;

   procedure Execute_PUSH_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_PUSH_HL_Impl;

   procedure Execute_PUSH_AF
     (GB : in out Gade.GB.GB_Type) renames Execute_PUSH_AF_Impl;

   procedure Execute_POP_BC
     (GB : in out Gade.GB.GB_Type) renames Execute_POP_BC_Impl;

   procedure Execute_POP_DE
     (GB : in out Gade.GB.GB_Type) renames Execute_POP_DE_Impl;

   procedure Execute_POP_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_POP_HL_Impl;

   procedure Execute_POP_AF
     (GB : in out Gade.GB.GB_Type) renames Execute_POP_AF_Impl;

end Gade.Dev.CPU.Instructions.Stack;
