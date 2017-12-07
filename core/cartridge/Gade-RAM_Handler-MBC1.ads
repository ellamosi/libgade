package Gade.RAM_Handler.MBC1 is

   subtype MBC1_Bank_Range is RAM_Bank_Range range 0 .. 4;

   type MBC1_RAM_Handler_Type is new RAM_Handler_Type with private;

   type MBC1_RAM_Handler_Access is access MBC1_RAM_Handler_Type;

   overriding
   procedure Create
     (Handler : out MBC1_RAM_Handler_Type);

   overriding
   procedure Read
     (Handler : in out MBC1_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte);

   overriding
   procedure Write
     (Handler : in out MBC1_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte);

   overriding
   procedure Switch_Banks
     (Handler : in out MBC1_RAM_Handler_Type;
      Bank    : RAM_Bank_Range);

   overriding
   procedure Set_Enabled
     (Handler : in out MBC1_RAM_Handler_Type;
      Enabled : Boolean);

private

   type MBC1_RAM_Handler_Type is new RAM_Handler_Type with record
      RAM     : RAM_Access;
      Bank    : RAM_Bank_Access;
      Enabled : Boolean;
   end record;

end Gade.RAM_Handler.MBC1;
