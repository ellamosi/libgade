package body Gade.Dev.External_RAM is

   procedure Reset (External_RAM : in out External_RAM_Type) is
   begin
      External_RAM.Enabled := False;
   end Reset;

   procedure Set_Enabled
     (External_RAM : in out External_RAM_Type;
      Enabled      : Boolean) is
   begin
      External_RAM.Enabled := Enabled;
   end Set_Enabled;

   procedure Switch_Banks
     (External_RAM : in out External_RAM_Type;
      Bank         : External_RAM_Bank_Range) is
   begin
      null;
   end Switch_Banks;

   procedure Read
     (External_RAM : in out External_RAM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : Word;
      Value        : out Byte) is
   begin
      null;
   end Read;

   procedure Write
     (External_RAM : in out External_RAM_Type;
      GB           : in out Gade.GB.GB_Type;
      Address      : Word;
      Value        : Byte) is
   begin
      null;
   end Write;

end Gade.Dev.External_RAM;
