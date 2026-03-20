with Gade.GB;

package Gade.Dev.CPU.Instructions.Arithmetic is
   package Instructions renames Gade.Dev.CPU.Instructions;

   type Carry_Type is private;

   ADC_Carry : constant Carry_Type;
   ADD_Carry : constant Carry_Type;
   SUB_Carry : constant Carry_Type;
   SBC_Carry : constant Carry_Type;

   procedure Add
     (CPU    : in out CPU_Context;
      Value  :        Byte;
      Result :    out Byte;
      Carry  :        Carry_Type);
   --  8 bit add

   procedure Add
     (CPU    : in out CPU_Context;
      Value  :        Word;
      Result :    out Word);
   --  16 Bit add

   procedure Add
     (CPU    : in out CPU_Context;
      Reg    : in out Word;
      Value  :        Byte);
   --  16 Bit add

   procedure Sub
     (CPU    : in out CPU_Context;
      Value  :        Byte;
      Result :    out Byte;
      Carry  :        Carry_Type);

   procedure Add_Offset
     (CPU       : in out CPU_Context;
      Address   : in out Word;
      Offset    :        Byte;
      Set_Flags :        Boolean);

   type Inc_Dec_Type is (INC, DEC);

   procedure Inc_Dec
     (CPU     : in out CPU_Context;
      Inc_Dec :        Inc_Dec_Type;
      Value   :        Byte;
      Result  :    out Byte);

   procedure Adjust_DAA
     (CPU : in out CPU_Context);

   generic
      Operation : Instructions.Inc_Dec_Op_Kind;
      Target    : Instructions.Byte_Target_Kind;
   procedure Inc_Dec_Byte
     (GB : in out Gade.GB.GB_Type);

   generic
      Operation : Instructions.Inc_Dec_Op_Kind;
      Target    : Instructions.Word_Register_Kind;
   procedure Inc_Dec_Word
     (GB : in out Gade.GB.GB_Type);

   generic
      Source : Instructions.Word_Register_Kind;
   procedure Add_HL
     (GB : in out Gade.GB.GB_Type);

   procedure ADD_SP_Imm8
     (GB : in out Gade.GB.GB_Type);

   procedure DAA
     (GB : in out Gade.GB.GB_Type);

private
   type Carry_Type is new Boolean;

   ADC_Carry : constant Carry_Type := True;
   ADD_Carry : constant Carry_Type := False;
   SUB_Carry : constant Carry_Type := False;
   SBC_Carry : constant Carry_Type := True;
end Gade.Dev.CPU.Instructions.Arithmetic;
