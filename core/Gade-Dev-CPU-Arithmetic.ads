with System;

package Gade.Dev.CPU.Arithmetic is

   type Carry_Type is private;

   ADC_Carry : constant Carry_Type;
   ADD_Carry : constant Carry_Type;

   procedure Do_Add
     (CPU    : in out CPU_Context;
      Value  :        Byte;
      Result :    out Byte;
      Carry  :        Carry_Type);
   -- 8 bit add

   procedure Do_Add
     (CPU    : in out CPU_Context;
      Value  :        Word;
      Result :    out Word);
   -- 16 Bit add

   procedure Do_Add
     (CPU    : in out CPU_Context;
      Reg    : in out Word;
      Value  :        Byte);
   -- 16 Bit add

   SUB_Carry : constant Carry_Type;
   SBC_Carry : constant Carry_Type;

   procedure Do_Sub
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

   procedure Do_Inc_Dec
     (CPU     : in out CPU_Context;
      Inc_Dec :        Inc_Dec_Type;
      Value   :        Byte;
      Result  :    out Byte);

   procedure Do_Daa
     (CPU     : in out CPU_Context);

private

   type Carry_Type is new Boolean;

   ADC_Carry : constant Carry_Type := True;
   SBC_Carry : constant Carry_Type := True;
   ADD_Carry : constant Carry_Type := False;
   SUB_Carry : constant Carry_Type := False;

end Gade.Dev.CPU.Arithmetic;
