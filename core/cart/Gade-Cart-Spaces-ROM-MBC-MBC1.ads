with Gade.Cart.Spaces.RAM;

package Gade.Cart.Spaces.ROM.MBC.MBC1 is

   type Handler_Type is new MBC.Handler_Type with private;
   type Handler_Access is access Handler_Type;

   function Create
     (ROM_Content : Cart.ROM.ROM_Content_Access;
      RAM_Handler : Cart.Spaces.RAM.Handler_Access)
      return Handler_Access;

   overriding
   procedure Reset (Handler : in out Handler_Type);

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

   type Handler_Type is new MBC.Handler_Type with record
      Banking_Mode     : Banking_Mode_Type;
      Low_Bank_Select  : Low_Bank_Select_Type;
      High_Bank_Select : High_Bank_Select_Type;
   end record;

   procedure Initialize
     (Handler     : out Handler_Type'Class;
      ROM_Content : Cart.ROM.ROM_Content_Access;
      RAM_Handler : Cart.Spaces.RAM.Handler_Access);

   overriding
   procedure Select_Bank
     (Handler : in out Handler_Type;
      Address : Bank_Select_Address;
      Value   : Byte);

   overriding
   procedure Enable_RAM
     (Handler : in out Handler_Type;
      Address : RAM_Enable_Address;
      Value   : Byte);

   overriding
   procedure Write_Special
     (Handler : in out Handler_Type;
      Value   : Byte);

   procedure Change_Banking_Mode
     (Handler  : in out Handler_Type;
      New_Mode : Banking_Mode_Type);

   procedure Select_Low_Bank
     (Handler : in out Handler_Type;
      Value   : Byte);

   procedure Select_High_Bank
     (Handler : in out Handler_Type;
      Value   : Byte);

   procedure Select_ROM_Bank (Handler : in out Handler_Type);

   procedure Select_RAM_Bank (Handler : in out Handler_Type);

end Gade.Cart.Spaces.ROM.MBC.MBC1;

