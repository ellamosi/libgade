package Gade.Cartridge.MBC.MBC1 is

   type MBC1_ROM_Handler_Type is new MBC_ROM_Handler_Type with private;

private

   type Banking_Mode_Type is (ROM, RAM);
   for Banking_Mode_Type use
     (ROM => 0,
      RAM => 1);

   Banking_Mode_Mask : constant := 16#01#;

   type Low_Bank_Select_Type  is mod 2**5;
   type High_Bank_Select_Type is mod 2**2;

   Low_Select_Mask  : constant := 16#1F#;
   High_Select_Mask : constant := 16#03#;

   subtype Low_Bank_Select_Address is
     Bank_Select_Address range 16#2000# .. 16#3FFF#;

   subtype High_Bank_Select_Address is
     Bank_Select_Address range 16#4000# .. 16#5FFF#;

   type MBC1_ROM_Handler_Type is new MBC_ROM_Handler_Type with record
      Banking_Mode     : Banking_Mode_Type;
      Low_Bank_Select  : Low_Bank_Select_Type;
      High_Bank_Select : High_Bank_Select_Type;
   end record;

   overriding
   procedure Create
     (Handler : out MBC1_ROM_Handler_Type;
      ROM     : ROM_Access);

   overriding
   procedure Select_Bank
     (Handler : in out MBC1_ROM_Handler_Type;
      Address : Bank_Select_Address;
      Value   : Byte);

   overriding
   procedure Write_Special
     (Handler : in out MBC1_ROM_Handler_Type;
      Value   : Byte);

   procedure Select_Low_Bank
     (Handler : in out MBC1_ROM_Handler_Type;
      Value   : Byte);

   procedure Select_High_Bank
     (Handler : in out MBC1_ROM_Handler_Type;
      Value   : Byte);

   procedure Select_ROM_Bank
     (Handler : in out MBC1_ROM_Handler_Type);

   procedure Select_RAM_Bank
     (Handler : in out MBC1_ROM_Handler_Type);

end Gade.Cartridge.MBC.MBC1;
