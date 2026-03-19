with Gade.GB;

package Gade.Dev.CPU.Instructions.Loads is
   package Instructions renames Gade.Dev.CPU.Instructions;

   procedure Execute_LD_BC_Imm16 is new Instructions.Execute_LD_Word
     (Dest   => Instructions.REG_BC,
      Source => Instructions.WSRC_Imm16);

   procedure Execute_LD_DE_Imm16 is new Instructions.Execute_LD_Word
     (Dest   => Instructions.REG_DE,
      Source => Instructions.WSRC_Imm16);

   procedure Execute_LD_HL_Imm16 is new Instructions.Execute_LD_Word
     (Dest   => Instructions.REG_HL,
      Source => Instructions.WSRC_Imm16);

   procedure Execute_LD_SP_Imm16 is new Instructions.Execute_LD_Word
     (Dest   => Instructions.REG_SP,
      Source => Instructions.WSRC_Imm16);

   procedure Execute_LD_Addr_Imm16_SP
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_LD_Addr_Imm16_SP;

   procedure Execute_LD_Addr_BC_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_BC,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_Addr_BC is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_BC);

   procedure Execute_LD_Addr_DE_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_DE,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_Addr_DE is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_DE);

   procedure Execute_LD_Addr_HL_Inc_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL_Inc,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_Addr_HL_Inc is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_HL_Inc);

   procedure Execute_LD_Addr_HL_Dec_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL_Dec,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_Addr_HL_Dec is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_HL_Dec);

   procedure Execute_LD_B_Imm8 is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_C_Imm8 is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_D_Imm8 is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_E_Imm8 is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_H_Imm8 is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_L_Imm8 is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_B_C is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_C);

   procedure Execute_LD_B_B is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_B);

   procedure Execute_LD_B_D is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_D);

   procedure Execute_LD_B_E is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_E);

   procedure Execute_LD_B_H is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_H);

   procedure Execute_LD_B_L is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_L);

   procedure Execute_LD_B_Addr_HL is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_B_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_A);

   procedure Execute_LD_C_B is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_B);

   procedure Execute_LD_C_C is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_C);

   procedure Execute_LD_C_D is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_D);

   procedure Execute_LD_C_E is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_E);

   procedure Execute_LD_C_H is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_H);

   procedure Execute_LD_C_L is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_L);

   procedure Execute_LD_C_Addr_HL is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_C_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_A);

   procedure Execute_LD_D_B is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_B);

   procedure Execute_LD_D_C is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_C);

   procedure Execute_LD_D_D is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_D);

   procedure Execute_LD_D_E is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_E);

   procedure Execute_LD_D_H is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_H);

   procedure Execute_LD_D_L is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_L);

   procedure Execute_LD_D_Addr_HL is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_D_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_A);

   procedure Execute_LD_E_B is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_B);

   procedure Execute_LD_E_C is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_C);

   procedure Execute_LD_E_D is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_D);

   procedure Execute_LD_E_E is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_E);

   procedure Execute_LD_E_H is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_H);

   procedure Execute_LD_E_L is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_L);

   procedure Execute_LD_E_Addr_HL is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_E_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_A);

   procedure Execute_LD_H_B is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_B);

   procedure Execute_LD_H_C is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_C);

   procedure Execute_LD_H_D is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_D);

   procedure Execute_LD_H_E is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_E);

   procedure Execute_LD_H_H is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_H);

   procedure Execute_LD_H_L is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_L);

   procedure Execute_LD_H_Addr_HL is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_H_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_A);

   procedure Execute_LD_L_B is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_B);

   procedure Execute_LD_L_C is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_C);

   procedure Execute_LD_L_D is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_D);

   procedure Execute_LD_L_E is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_E);

   procedure Execute_LD_L_H is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_H);

   procedure Execute_LD_L_L is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_L);

   procedure Execute_LD_L_Addr_HL is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_L_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_A);

   procedure Execute_LD_Addr_HL_B is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_B);

   procedure Execute_LD_Addr_HL_C is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_C);

   procedure Execute_LD_Addr_HL_D is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_D);

   procedure Execute_LD_Addr_HL_E is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_E);

   procedure Execute_LD_Addr_HL_H is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_H);

   procedure Execute_LD_Addr_HL_L is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_L);

   procedure Execute_LD_Addr_HL_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_B is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_B);

   procedure Execute_LD_A_C is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_C);

   procedure Execute_LD_A_D is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_D);

   procedure Execute_LD_A_E is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_E);

   procedure Execute_LD_A_H is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_H);

   procedure Execute_LD_A_L is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_L);

   procedure Execute_LD_A_Addr_HL is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_A_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_Imm8 is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_Addr_HL_Imm8 is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_High_Addr_Imm8_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_High_Addr_Imm8,
      Source => Instructions.SRC_A);

   procedure Execute_LD_High_Addr_C_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_High_Addr_C,
      Source => Instructions.SRC_A);

   procedure Execute_LD_Addr_Imm16_A is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_Imm16,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_High_Addr_Imm8 is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_High_Addr_Imm8);

   procedure Execute_LD_A_High_Addr_C is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_High_Addr_C);

   procedure Execute_LD_SP_HL is new Instructions.Execute_LD_Word
     (Dest   => Instructions.REG_SP,
      Source => Instructions.WSRC_HL);

   procedure Execute_LD_A_Addr_Imm16 is new Instructions.Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_Imm16);

   procedure Execute_LD_HL_SP_Plus_Imm8
     (GB : in out Gade.GB.GB_Type) renames Instructions.Execute_LD_HL_SP_Plus_Imm8;

end Gade.Dev.CPU.Instructions.Loads;
