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

   procedure Execute_LD_Addr_Imm16_SP
     (GB : in out Gade.GB.GB_Type);

   procedure Execute_LD_HL_SP_Plus_Imm8
     (GB : in out Gade.GB.GB_Type);

end Gade.Dev.CPU.Instructions.Loads;
