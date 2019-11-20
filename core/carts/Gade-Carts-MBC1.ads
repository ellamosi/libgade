private with Gade.Carts.Mixins.MBC;

package Gade.Carts.MBC1 is

   type MBC1_Cart is new Cart with private;

   type MBC1_Cart_Access is access MBC1_Cart;

   subtype MBC1_Cart_NN_Access is not null MBC1_Cart_Access;

   overriding
   procedure Reset (C : in out MBC1_Cart);

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

   package MBC_Mixin is new Gade.Carts.Mixins.MBC
     (Base_Cart => Cart,
      ROM_Banks => ROM_Bank_Count,
      RAM_Banks => RAM_Bank_Count);
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
