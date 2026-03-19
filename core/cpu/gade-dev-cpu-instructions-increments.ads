package Gade.Dev.CPU.Instructions.Increments is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Execute_INC_BC is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_BC);

   procedure Execute_INC_B is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_B);

   procedure Execute_DEC_B is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_B);

   procedure Execute_DEC_BC is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_BC);

   procedure Execute_INC_C is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_C);

   procedure Execute_DEC_C is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_C);

   procedure Execute_INC_DE is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_DE);

   procedure Execute_INC_D is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_D);

   procedure Execute_DEC_D is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_D);

   procedure Execute_DEC_DE is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_DE);

   procedure Execute_INC_E is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_E);

   procedure Execute_DEC_E is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_E);

   procedure Execute_INC_H is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_H);

   procedure Execute_DEC_H is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_H);

   procedure Execute_INC_L is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_L);

   procedure Execute_DEC_L is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_L);

   procedure Execute_INC_SP is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_SP);

   procedure Execute_INC_Addr_HL is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_DEC_Addr_HL is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_Addr_HL);

   procedure Execute_INC_HL is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_INC,
      Target    => Instructions.REG_HL);

   procedure Execute_DEC_HL is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_HL);

   procedure Execute_DEC_SP is new Instructions.Execute_Inc_Dec_Word
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.REG_SP);

   procedure Execute_INC_A is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_INC,
      Target    => Instructions.DST_A);

   procedure Execute_DEC_A is new Instructions.Execute_Inc_Dec_Byte
     (Operation => Instructions.OP_DEC,
      Target    => Instructions.DST_A);

end Gade.Dev.CPU.Instructions.Increments;
