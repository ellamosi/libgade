private with Gade.Cart.C2.Mixins.MBC;
private with Gade.Cart.C2.Mixins.Banked_ROM;
private with Gade.Cart.C2.Mixins.Plain_RAM;
private with Gade.Cart.C2.Mixins.Toggled_RAM;

package Gade.Cart.C2.MBC2 is

   type MBC2_Cart is new Cart with private;

private

   type ROM_Bank_Index is range 0 .. 15;

   Max_RAM_Bytes : constant := 512;

   Bank_Select_Accept_Mask : constant Word := 16#0100#;
   Bank_Select_Mask        : constant Byte := 16#0F#;

   RAM_Enable_Accept_Mask : constant Word := 16#0100#;
   RAM_Enable_Mask        : constant Byte := 16#0F#;
   RAM_Enable_Value       : constant Byte := 16#0A#;

   RAM_Content_Mask : constant Byte := 16#F0#;

   package Banked_ROM_Mixin is new Mixins.Banked_ROM (Cart, ROM_Bank_Index);
   use Banked_ROM_Mixin;
   package Plain_RAM_Mixin is new Mixins.Plain_RAM (Banked_ROM_Cart);
   use Plain_RAM_Mixin;
   package Toggled_RAM_Mixin is new Mixins.Toggled_RAM (Plain_RAM_Cart);
   use Toggled_RAM_Mixin;
   package MBC_Mixin is new Mixins.MBC (Toggled_RAM_Cart);
   use MBC_Mixin;

   type MBC2_Cart is new MBC_Mixin.MBC_Cart with null record;

   overriding
   procedure Read_RAM
     (C       : in out MBC2_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte);

   overriding
   procedure Write_RAM
     (C       : in out MBC2_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte);

   overriding
   procedure Enable_RAM
     (C       : in out MBC2_Cart;
      Address : RAM_Enable_Address;
      Value   : Byte);

   overriding
   procedure Select_Bank
     (C       : in out MBC2_Cart;
      Address : Bank_Select_Address;
      Value   : Byte);

end Gade.Cart.C2.MBC2;
