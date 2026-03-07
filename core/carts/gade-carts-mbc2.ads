private with Gade.Carts.Mixins.MBC;

package Gade.Carts.MBC2 is

   type MBC2_Cart is new Cart with private;

   type MBC2_Cart_Access is access MBC2_Cart;

   subtype MBC2_Cart_NN_Access is not null MBC2_Cart_Access;

   overriding
   procedure Write_ROM
     (C       : in out MBC2_Cart;
      Address : External_ROM_IO_Address;
      Value   : Byte);

private

   ROM_Bank_Count : constant := 16;
   RAM_Bank_Count : constant := 1;

   --  Actual size is 512x4 bits, but this is going to be used to allocate
   --  1 byte per addressable 4 bits of RAM, at least for now.
   RAM_Bytes : constant := 512;

   Bank_Select_Mask : constant Byte := 16#0F#;

   RAM_Enable_Accept_Mask : constant Word := 16#0100#;
   RAM_Enable_Mask        : constant Byte := 16#0F#;
   RAM_Enable_Value       : constant Byte := 16#0A#;

   subtype Lower_ROM_IO_Address is
     External_ROM_IO_Address range 16#0000# .. 16#3FFF#;

   package MBC_Mixin is new Gade.Carts.Mixins.MBC
     (Base_Cart => Cart,
      ROM_Banks => ROM_Bank_Count,
      RAM_Banks => RAM_Bank_Count);
   use MBC_Mixin;

   type MBC2_Cart is new MBC_Cart with null record;

   procedure Select_Bank
     (C     : in out MBC2_Cart;
      Value : Byte);

end Gade.Carts.MBC2;
