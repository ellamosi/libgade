private with Gade.Carts.Mixins.MBC;
private with Gade.Carts.Mixins.Banked.ROM;
private with Gade.Carts.Mixins.Banked.RAM;

package Gade.Carts.MBC1 is

   type MBC1_Cart is new Cart with private;

   type MBC1_Cart_Access is access MBC1_Cart;

   subtype MBC1_Cart_NN_Access is not null MBC1_Cart_Access;

private

   ROM_Bank_Count : constant := 128;
   RAM_Bank_Count : constant := 4;

   type Banking_Mode_Type is (ROM, RAM);
   for Banking_Mode_Type use
     (ROM => 0,
      RAM => 1);

   Banking_Mode_Mask : constant := 16#01#;

   type Low_Bank_Select_Type  is mod 2**5;
   type High_Bank_Select_Type is mod 2**2;

   Low_Select_Mask  : constant := 16#1F#;
   High_Select_Mask : constant := 16#03#;

   RAM_Enable_Mask   : constant := 16#0F#;
   RAM_Enable_Value  : constant := 16#0A#;
   RAM_Disable_Value : constant := 16#00#;

   package Banked_ROM_Mixin is new Gade.Carts.Mixins.Banked.ROM
     (Base_Cart => Cart,
      Banks     => ROM_Bank_Count);
   package Banked_RAM_Mixin is new Gade.Carts.Mixins.Banked.RAM
     (Base_Cart => Banked_ROM_Mixin.Banked_ROM_Cart,
      Banks     => RAM_Bank_Count);
   package MBC_Mixin is new Mixins.MBC
     (Base_Cart => Banked_RAM_Mixin.Banked_RAM_Cart);
   use MBC_Mixin;

   type MBC1_Cart is new MBC_Cart with record
      Banking_Mode     : Banking_Mode_Type;
      Low_Bank_Select  : Low_Bank_Select_Type;
      High_Bank_Select : High_Bank_Select_Type;
   end record;

   subtype Low_Bank_Select_Address is
     Bank_Select_Address range 16#2000# .. 16#3FFF#;

   subtype High_Bank_Select_Address is
     Bank_Select_Address range 16#4000# .. 16#5FFF#;

   overriding
   procedure Enable_RAM
     (C       : in out MBC1_Cart;
      Address : RAM_Enable_Address;
      Value   : Byte);

   overriding
   procedure Write_Special
     (C       : in out MBC1_Cart;
      Value   : Byte);

   overriding
   procedure Select_Bank
     (C       : in out MBC1_Cart;
      Address : Bank_Select_Address;
      Value   : Byte);

   procedure Select_Low_Bank
     (C     : in out MBC1_Cart;
      Value : Byte);

   procedure Select_High_Bank
     (C     : in out MBC1_Cart;
      Value : Byte);

   procedure Select_ROM_Bank (C : in out MBC1_Cart);

   procedure Select_RAM_Bank (C : in out MBC1_Cart);

   procedure Change_Banking_Mode
     (C        : in out MBC1_Cart;
      New_Mode : Banking_Mode_Type);

end Gade.Carts.MBC1;
