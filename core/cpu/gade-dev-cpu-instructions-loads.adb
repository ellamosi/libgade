with Gade.Dev.CPU.Instructions.Arithmetic;

package body Gade.Dev.CPU.Instructions.Loads is

   procedure Execute_LD_Addr_Imm16_SP
     (GB : in out Gade.GB.GB_Type) is
   begin
      declare
         Address : constant Word := Instructions.Fetch_Imm16 (GB);
      begin
         Instructions.Bus_Write_Byte (GB, Address, Byte (GB.CPU.Regs.SP and 16#00FF#));
         Instructions.Bus_Write_Byte (GB, Address + 1, Byte (GB.CPU.Regs.SP / 2**8));
      end;
   end Execute_LD_Addr_Imm16_SP;

   procedure Execute_LD_HL_SP_Plus_Imm8
     (GB : in out Gade.GB.GB_Type) is
      Value : Word := GB.CPU.Regs.SP;
   begin
      Gade.Dev.CPU.Instructions.Arithmetic.Do_Add
        (GB.CPU, Value, Instructions.Fetch_Imm8 (GB));
      Instructions.Internal_Cycle (GB);
      GB.CPU.Regs.HL := Value;
   end Execute_LD_HL_SP_Plus_Imm8;


   procedure Execute_LD_Byte
     (GB : in out Gade.GB.GB_Type) is
      Value : constant Byte := Instructions.Fetch_Source (GB, Source);
   begin
      Instructions.Store_Target (GB, Dest, Value);
      Instructions.Adjust_HL_Auto (GB, Source);
      Instructions.Adjust_HL_Auto (GB, Dest);
   end Execute_LD_Byte;

   procedure Execute_LD_Word
     (GB : in out Gade.GB.GB_Type) is
   begin
      Instructions.Write_Word_Register (GB, Dest, Instructions.Read_Word_Source (GB, Source));

      if Dest = Instructions.REG_SP and then Source = Instructions.WSRC_HL then
         Instructions.Internal_Cycle (GB);
      end if;
   end Execute_LD_Word;

   procedure Execute_LD_BC_Imm16_Impl is new Execute_LD_Word
     (Dest   => Instructions.REG_BC,
      Source => Instructions.WSRC_Imm16);

   procedure Execute_LD_DE_Imm16_Impl is new Execute_LD_Word
     (Dest   => Instructions.REG_DE,
      Source => Instructions.WSRC_Imm16);

   procedure Execute_LD_HL_Imm16_Impl is new Execute_LD_Word
     (Dest   => Instructions.REG_HL,
      Source => Instructions.WSRC_Imm16);

   procedure Execute_LD_SP_Imm16_Impl is new Execute_LD_Word
     (Dest   => Instructions.REG_SP,
      Source => Instructions.WSRC_Imm16);

   procedure Execute_LD_SP_HL_Impl is new Execute_LD_Word
     (Dest   => Instructions.REG_SP,
      Source => Instructions.WSRC_HL);

   procedure Execute_LD_Addr_BC_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_BC,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_Addr_BC_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_BC);

   procedure Execute_LD_Addr_DE_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_DE,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_Addr_DE_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_DE);

   procedure Execute_LD_Addr_HL_Inc_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL_Inc,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_Addr_HL_Inc_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_HL_Inc);

   procedure Execute_LD_Addr_HL_Dec_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL_Dec,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_Addr_HL_Dec_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_HL_Dec);

   procedure Execute_LD_B_Imm8_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_C_Imm8_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_D_Imm8_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_E_Imm8_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_H_Imm8_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_L_Imm8_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_B_C_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_C);

   procedure Execute_LD_B_B_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_B);

   procedure Execute_LD_B_D_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_D);

   procedure Execute_LD_B_E_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_E);

   procedure Execute_LD_B_H_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_H);

   procedure Execute_LD_B_L_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_L);

   procedure Execute_LD_B_Addr_HL_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_B_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_B,
      Source => Instructions.SRC_A);

   procedure Execute_LD_C_B_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_B);

   procedure Execute_LD_C_C_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_C);

   procedure Execute_LD_C_D_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_D);

   procedure Execute_LD_C_E_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_E);

   procedure Execute_LD_C_H_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_H);

   procedure Execute_LD_C_L_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_L);

   procedure Execute_LD_C_Addr_HL_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_C_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_C,
      Source => Instructions.SRC_A);

   procedure Execute_LD_D_B_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_B);

   procedure Execute_LD_D_C_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_C);

   procedure Execute_LD_D_D_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_D);

   procedure Execute_LD_D_E_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_E);

   procedure Execute_LD_D_H_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_H);

   procedure Execute_LD_D_L_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_L);

   procedure Execute_LD_D_Addr_HL_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_D_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_D,
      Source => Instructions.SRC_A);

   procedure Execute_LD_E_B_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_B);

   procedure Execute_LD_E_C_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_C);

   procedure Execute_LD_E_D_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_D);

   procedure Execute_LD_E_E_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_E);

   procedure Execute_LD_E_H_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_H);

   procedure Execute_LD_E_L_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_L);

   procedure Execute_LD_E_Addr_HL_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_E_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_E,
      Source => Instructions.SRC_A);

   procedure Execute_LD_H_B_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_B);

   procedure Execute_LD_H_C_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_C);

   procedure Execute_LD_H_D_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_D);

   procedure Execute_LD_H_E_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_E);

   procedure Execute_LD_H_H_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_H);

   procedure Execute_LD_H_L_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_L);

   procedure Execute_LD_H_Addr_HL_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_H_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_H,
      Source => Instructions.SRC_A);

   procedure Execute_LD_L_B_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_B);

   procedure Execute_LD_L_C_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_C);

   procedure Execute_LD_L_D_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_D);

   procedure Execute_LD_L_E_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_E);

   procedure Execute_LD_L_H_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_H);

   procedure Execute_LD_L_L_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_L);

   procedure Execute_LD_L_Addr_HL_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_L_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_L,
      Source => Instructions.SRC_A);

   procedure Execute_LD_Addr_HL_B_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_B);

   procedure Execute_LD_Addr_HL_C_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_C);

   procedure Execute_LD_Addr_HL_D_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_D);

   procedure Execute_LD_Addr_HL_E_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_E);

   procedure Execute_LD_Addr_HL_H_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_H);

   procedure Execute_LD_Addr_HL_L_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_L);

   procedure Execute_LD_Addr_HL_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_B_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_B);

   procedure Execute_LD_A_C_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_C);

   procedure Execute_LD_A_D_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_D);

   procedure Execute_LD_A_E_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_E);

   procedure Execute_LD_A_H_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_H);

   procedure Execute_LD_A_L_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_L);

   procedure Execute_LD_A_Addr_HL_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_HL);

   procedure Execute_LD_A_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_Imm8_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_Addr_HL_Imm8_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_HL,
      Source => Instructions.SRC_Imm8);

   procedure Execute_LD_High_Addr_Imm8_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_High_Addr_Imm8,
      Source => Instructions.SRC_A);

   procedure Execute_LD_High_Addr_C_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_High_Addr_C,
      Source => Instructions.SRC_A);

   procedure Execute_LD_Addr_Imm16_A_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_Addr_Imm16,
      Source => Instructions.SRC_A);

   procedure Execute_LD_A_High_Addr_Imm8_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_High_Addr_Imm8);

   procedure Execute_LD_A_High_Addr_C_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_High_Addr_C);

   procedure Execute_LD_A_Addr_Imm16_Impl is new Execute_LD_Byte
     (Dest   => Instructions.DST_A,
      Source => Instructions.SRC_Addr_Imm16);

   procedure Execute_LD_BC_Imm16
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_BC_Imm16_Impl;
   procedure Execute_LD_DE_Imm16
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_DE_Imm16_Impl;
   procedure Execute_LD_HL_Imm16
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_HL_Imm16_Impl;
   procedure Execute_LD_SP_Imm16
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_SP_Imm16_Impl;
   procedure Execute_LD_SP_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_SP_HL_Impl;
   procedure Execute_LD_Addr_BC_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_BC_A_Impl;
   procedure Execute_LD_A_Addr_BC
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_Addr_BC_Impl;
   procedure Execute_LD_Addr_DE_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_DE_A_Impl;
   procedure Execute_LD_A_Addr_DE
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_Addr_DE_Impl;
   procedure Execute_LD_Addr_HL_Inc_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_HL_Inc_A_Impl;
   procedure Execute_LD_A_Addr_HL_Inc
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_Addr_HL_Inc_Impl;
   procedure Execute_LD_Addr_HL_Dec_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_HL_Dec_A_Impl;
   procedure Execute_LD_A_Addr_HL_Dec
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_Addr_HL_Dec_Impl;
   procedure Execute_LD_B_Imm8
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_B_Imm8_Impl;
   procedure Execute_LD_C_Imm8
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_C_Imm8_Impl;
   procedure Execute_LD_D_Imm8
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_D_Imm8_Impl;
   procedure Execute_LD_E_Imm8
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_E_Imm8_Impl;
   procedure Execute_LD_H_Imm8
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_H_Imm8_Impl;
   procedure Execute_LD_L_Imm8
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_L_Imm8_Impl;
   procedure Execute_LD_B_C
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_B_C_Impl;
   procedure Execute_LD_B_B
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_B_B_Impl;
   procedure Execute_LD_B_D
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_B_D_Impl;
   procedure Execute_LD_B_E
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_B_E_Impl;
   procedure Execute_LD_B_H
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_B_H_Impl;
   procedure Execute_LD_B_L
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_B_L_Impl;
   procedure Execute_LD_B_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_B_Addr_HL_Impl;
   procedure Execute_LD_B_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_B_A_Impl;
   procedure Execute_LD_C_B
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_C_B_Impl;
   procedure Execute_LD_C_C
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_C_C_Impl;
   procedure Execute_LD_C_D
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_C_D_Impl;
   procedure Execute_LD_C_E
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_C_E_Impl;
   procedure Execute_LD_C_H
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_C_H_Impl;
   procedure Execute_LD_C_L
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_C_L_Impl;
   procedure Execute_LD_C_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_C_Addr_HL_Impl;
   procedure Execute_LD_C_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_C_A_Impl;
   procedure Execute_LD_D_B
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_D_B_Impl;
   procedure Execute_LD_D_C
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_D_C_Impl;
   procedure Execute_LD_D_D
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_D_D_Impl;
   procedure Execute_LD_D_E
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_D_E_Impl;
   procedure Execute_LD_D_H
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_D_H_Impl;
   procedure Execute_LD_D_L
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_D_L_Impl;
   procedure Execute_LD_D_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_D_Addr_HL_Impl;
   procedure Execute_LD_D_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_D_A_Impl;
   procedure Execute_LD_E_B
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_E_B_Impl;
   procedure Execute_LD_E_C
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_E_C_Impl;
   procedure Execute_LD_E_D
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_E_D_Impl;
   procedure Execute_LD_E_E
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_E_E_Impl;
   procedure Execute_LD_E_H
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_E_H_Impl;
   procedure Execute_LD_E_L
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_E_L_Impl;
   procedure Execute_LD_E_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_E_Addr_HL_Impl;
   procedure Execute_LD_E_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_E_A_Impl;
   procedure Execute_LD_H_B
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_H_B_Impl;
   procedure Execute_LD_H_C
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_H_C_Impl;
   procedure Execute_LD_H_D
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_H_D_Impl;
   procedure Execute_LD_H_E
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_H_E_Impl;
   procedure Execute_LD_H_H
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_H_H_Impl;
   procedure Execute_LD_H_L
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_H_L_Impl;
   procedure Execute_LD_H_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_H_Addr_HL_Impl;
   procedure Execute_LD_H_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_H_A_Impl;
   procedure Execute_LD_L_B
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_L_B_Impl;
   procedure Execute_LD_L_C
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_L_C_Impl;
   procedure Execute_LD_L_D
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_L_D_Impl;
   procedure Execute_LD_L_E
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_L_E_Impl;
   procedure Execute_LD_L_H
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_L_H_Impl;
   procedure Execute_LD_L_L
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_L_L_Impl;
   procedure Execute_LD_L_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_L_Addr_HL_Impl;
   procedure Execute_LD_L_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_L_A_Impl;
   procedure Execute_LD_Addr_HL_B
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_HL_B_Impl;
   procedure Execute_LD_Addr_HL_C
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_HL_C_Impl;
   procedure Execute_LD_Addr_HL_D
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_HL_D_Impl;
   procedure Execute_LD_Addr_HL_E
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_HL_E_Impl;
   procedure Execute_LD_Addr_HL_H
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_HL_H_Impl;
   procedure Execute_LD_Addr_HL_L
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_HL_L_Impl;
   procedure Execute_LD_Addr_HL_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_HL_A_Impl;
   procedure Execute_LD_A_B
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_B_Impl;
   procedure Execute_LD_A_C
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_C_Impl;
   procedure Execute_LD_A_D
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_D_Impl;
   procedure Execute_LD_A_E
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_E_Impl;
   procedure Execute_LD_A_H
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_H_Impl;
   procedure Execute_LD_A_L
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_L_Impl;
   procedure Execute_LD_A_Addr_HL
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_Addr_HL_Impl;
   procedure Execute_LD_A_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_A_Impl;
   procedure Execute_LD_A_Imm8
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_Imm8_Impl;
   procedure Execute_LD_Addr_HL_Imm8
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_HL_Imm8_Impl;
   procedure Execute_LD_High_Addr_Imm8_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_High_Addr_Imm8_A_Impl;
   procedure Execute_LD_High_Addr_C_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_High_Addr_C_A_Impl;
   procedure Execute_LD_Addr_Imm16_A
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_Addr_Imm16_A_Impl;
   procedure Execute_LD_A_High_Addr_Imm8
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_High_Addr_Imm8_Impl;
   procedure Execute_LD_A_High_Addr_C
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_High_Addr_C_Impl;
   procedure Execute_LD_A_Addr_Imm16
     (GB : in out Gade.GB.GB_Type) renames Execute_LD_A_Addr_Imm16_Impl;

end Gade.Dev.CPU.Instructions.Loads;
