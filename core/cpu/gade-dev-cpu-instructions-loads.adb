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

end Gade.Dev.CPU.Instructions.Loads;
