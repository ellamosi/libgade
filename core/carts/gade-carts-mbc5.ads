private with Gade.Carts.Mixins.MBC;

package Gade.Carts.MBC5 is

   type MBC5_Cart is new Cart with private;

   type MBC5_Cart_Access is access MBC5_Cart;

   subtype MBC5_Cart_NN_Access is not null MBC5_Cart_Access;

   overriding
   procedure Reset (C : in out MBC5_Cart);

private

   ROM_Bank_Count : constant := 512;
   RAM_Bank_Count : constant := 16;

   type Low_ROM_Bank_Select_Type is mod 2**8;
   type High_ROM_Bank_Select_Type is mod 2**1;
   type RAM_Bank_Select_Type is mod 2**4;

   Low_ROM_Index_Mask    : constant Byte := 16#FF#;
   High_ROM_Index_Mask   : constant Byte := 16#01#;
   RAM_Index_Mask        : constant Byte := 16#0F#;
   Rumble_Bit_Mask       : constant Byte := 16#08#;
   Rumble_RAM_Index_Mask : constant Byte := 16#07#;

   package MBC_Mixin is new
     Gade.Carts.Mixins.MBC
       (Base_Cart => Cart,
        ROM_Banks => ROM_Bank_Count,
        RAM_Banks => RAM_Bank_Count);
   use MBC_Mixin;

   type MBC5_Cart is new MBC_Cart with record
      Low_ROM_Bank_Select  : Low_ROM_Bank_Select_Type;
      High_ROM_Bank_Select : High_ROM_Bank_Select_Type;
      RAM_Bank_Select      : RAM_Bank_Select_Type;
      Has_Rumble           : Boolean;
      Rumble_Enabled       : Boolean;
   end record;

   subtype Low_ROM_Bank_Select_Address is Bank_Select_Address range 16#2000# .. 16#2FFF#;

   subtype High_ROM_Bank_Select_Address is Bank_Select_Address range 16#3000# .. 16#3FFF#;

   subtype RAM_Bank_Select_Address is Bank_Select_Address range 16#4000# .. 16#5FFF#;

   overriding
   procedure Select_Bank
     (C : in out MBC5_Cart; Address : Bank_Select_Address; Value : Byte);

   procedure Select_Low_ROM_Bank (C : in out MBC5_Cart; Value : Byte);

   procedure Select_High_ROM_Bank (C : in out MBC5_Cart; Value : Byte);

   procedure Select_ROM_Bank (C : in out MBC5_Cart);

   procedure Select_RAM_Bank (C : in out MBC5_Cart; Value : Byte);

end Gade.Carts.MBC5;
