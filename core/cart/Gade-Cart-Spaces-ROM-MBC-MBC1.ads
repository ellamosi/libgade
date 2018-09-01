with Gade.Cart.Spaces.RAM;

package Gade.Cart.Spaces.ROM.MBC.MBC1 is

   type MBC1_ROM_Space_Type is new MBC_ROM_Space_Type with private;

   type MBC1_ROM_Space_Access is access MBC1_ROM_Space_Type;

   function Create
     (ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Handler : Gade.Cart.Spaces.RAM.RAM_Space_Access)
      return MBC1_ROM_Space_Access;

   overriding
   procedure Reset
     (Space : in out MBC1_ROM_Space_Type);

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

   type MBC1_ROM_Space_Type is new MBC_ROM_Space_Type with record
      Banking_Mode     : Banking_Mode_Type;
      Low_Bank_Select  : Low_Bank_Select_Type;
      High_Bank_Select : High_Bank_Select_Type;
   end record;

   procedure Initialize
     (Space       : out MBC1_ROM_Space_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Space   : Gade.Cart.Spaces.RAM.RAM_Space_Access);

   overriding
   procedure Select_Bank
     (Space   : in out MBC1_ROM_Space_Type;
      Address : Bank_Select_Address;
      Value   : Byte);

   overriding
   procedure Enable_RAM
     (Space   : in out MBC1_ROM_Space_Type;
      Address : RAM_Enable_Address;
      Value   : Byte);

   overriding
   procedure Write_Special
     (Space : in out MBC1_ROM_Space_Type;
      Value : Byte);

   procedure Change_Banking_Mode
     (Space    : in out MBC1_ROM_Space_Type;
      New_Mode : Banking_Mode_Type);

   procedure Select_Low_Bank
     (Space : in out MBC1_ROM_Space_Type;
      Value : Byte);

   procedure Select_High_Bank
     (Space : in out MBC1_ROM_Space_Type;
      Value : Byte);

   procedure Select_ROM_Bank
     (Space : in out MBC1_ROM_Space_Type);

   procedure Select_RAM_Bank
     (Space : in out MBC1_ROM_Space_Type);

end Gade.Cart.Spaces.ROM.MBC.MBC1;
