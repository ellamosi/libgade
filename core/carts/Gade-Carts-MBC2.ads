--  private with Gade.Carts.Mixins.MBC;
private with Gade.Carts.Mixins.Banked.ROM;
private with Gade.Carts.Mixins.Banked.RAM;

package Gade.Carts.MBC2 is

   type MBC2_Cart is new Cart with private;

   type MBC2_Cart_Access is access MBC2_Cart;

   subtype MBC2_Cart_NN_Access is not null MBC2_Cart_Access;

   overriding
   procedure Reset (C : in out MBC2_Cart);

private

   ROM_Bank_Count : constant := 16;
   RAM_Bank_Count : constant := 1;

   --  Actual size is 512x4 bits, but this is going to be used to allocate
   --  1 byte per addressable 4 bits of RAM, at least for now.
   RAM_Bytes : constant := 512;

   Bank_Select_Accept_Mask : constant Word := 16#0100#;
   Bank_Select_Mask        : constant Byte := 16#0F#;

   RAM_Enable_Accept_Mask : constant Word := 16#0100#;
   RAM_Enable_Mask        : constant Byte := 16#0F#;
   RAM_Enable_Value       : constant Byte := 16#0A#;

   subtype Lower_ROM_IO_Address is
     External_ROM_IO_Address range 16#0000# .. 16#3FFF#;

   package Banked_ROM_Mixin is new Gade.Carts.Mixins.Banked.ROM
     (Base_Cart => Cart,
      Banks     => ROM_Bank_Count);
   package MBC2_RAM_Mixin is new Gade.Carts.Mixins.Banked.RAM
     (Base_Cart => Banked_ROM_Mixin.Banked_ROM_Cart,
      Banks     => RAM_Bank_Count);
--     package MBC_Mixin is new Mixins.MBC
--       (Base_Cart => MBC2_RAM_Mixin.Banked_RAM_Cart);
--     use MBC_Mixin;
   use MBC2_RAM_Mixin;

   --  type MBC2_Cart is new MBC_Mixin.MBC_Cart with null record;
   type MBC2_Cart is new MBC2_RAM_Mixin.Banked_RAM_Cart with null record;

   overriding
   procedure Write_ROM
     (C       : in out MBC2_Cart;
      Address : External_ROM_IO_Address;
      Value   : Byte);

   procedure Enable_RAM
     (C       : in out MBC2_Cart;
      --  Address : RAM_Enable_Address;
      Value   : Byte);

   procedure Select_Bank
     (C       : in out MBC2_Cart;
      --  Address : Bank_Select_Address;
      Value   : Byte);

end Gade.Carts.MBC2;
