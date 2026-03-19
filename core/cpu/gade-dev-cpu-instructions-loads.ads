with Gade.GB;

package Gade.Dev.CPU.Instructions.Loads is
   package Instructions renames Gade.Dev.CPU.Instructions;

   generic
      Dest   : Instructions.Byte_Target_Kind;
      Source : Instructions.Byte_Source_Kind;
   procedure Execute_LD_Byte
     (GB : in out Gade.GB.GB_Type);

   generic
      Dest   : Instructions.Word_Register_Kind;
      Source : Instructions.Word_Source_Kind;
   procedure Execute_LD_Word
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_BC_Imm16
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_DE_Imm16
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_HL_Imm16
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_SP_Imm16
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_Imm16_SP
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_BC_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_Addr_BC
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_DE_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_Addr_DE
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_HL_Inc_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_Addr_HL_Inc
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_HL_Dec_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_Addr_HL_Dec
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_B_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_C_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_D_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_E_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_H_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_L_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_B_C
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_B_B
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_B_D
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_B_E
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_B_H
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_B_L
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_B_Addr_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_B_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_C_B
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_C_C
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_C_D
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_C_E
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_C_H
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_C_L
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_C_Addr_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_C_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_D_B
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_D_C
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_D_D
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_D_E
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_D_H
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_D_L
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_D_Addr_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_D_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_E_B
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_E_C
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_E_D
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_E_E
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_E_H
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_E_L
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_E_Addr_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_E_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_H_B
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_H_C
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_H_D
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_H_E
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_H_H
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_H_L
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_H_Addr_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_H_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_L_B
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_L_C
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_L_D
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_L_E
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_L_H
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_L_L
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_L_Addr_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_L_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_HL_B
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_HL_C
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_HL_D
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_HL_E
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_HL_H
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_HL_L
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_HL_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_B
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_C
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_D
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_E
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_H
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_L
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_Addr_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_HL_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_High_Addr_Imm8_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_High_Addr_C_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_Addr_Imm16_A
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_High_Addr_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_High_Addr_C
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_SP_HL
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_A_Addr_Imm16
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_HL_SP_Plus_Imm8
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions.Loads;
