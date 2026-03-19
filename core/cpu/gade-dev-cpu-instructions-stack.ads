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

   procedure Execute_PUSH_BC
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_PUSH_DE
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_PUSH_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_PUSH_AF
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_POP_BC
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_POP_DE
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_POP_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_POP_AF
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions.Stack;
