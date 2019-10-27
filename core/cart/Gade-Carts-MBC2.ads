private with Gade.Carts.Mixins.MBC;
private with Gade.Carts.Mixins.Banked.ROM;
private with Gade.Carts.Mixins.Banked.RAM;

package Gade.Carts.MBC2 is

   type MBC2_Cart is new Cart with private;

   type MBC2_Cart_Access is access MBC2_Cart;

   subtype MBC2_Cart_NN_Access is not null MBC2_Cart_Access;

private

   ROM_Bank_Count : constant := 16;
   RAM_Bank_Count : constant := 1;

   --  Max_RAM_Bytes : constant := 512;

   Bank_Select_Accept_Mask : constant Word := 16#0100#;
   Bank_Select_Mask        : constant Byte := 16#0F#;

   RAM_Enable_Accept_Mask : constant Word := 16#0100#;
   RAM_Enable_Mask        : constant Byte := 16#0F#;
   RAM_Enable_Value       : constant Byte := 16#0A#;

   package Banked_ROM_Mixin is new Gade.Carts.Mixins.Banked.ROM
     (Base_Cart => Cart,
      Banks     => ROM_Bank_Count);
   package MBC2_RAM_Mixin is new Gade.Carts.Mixins.Banked.RAM
     (Base_Cart => Banked_ROM_Mixin.Banked_ROM_Cart,
      Banks     => RAM_Bank_Count);
   package MBC_Mixin is new Mixins.MBC
     (Base_Cart => MBC2_RAM_Mixin.Banked_RAM_Cart);
   use Banked_ROM_Mixin, MBC_Mixin;

   type MBC2_Cart is new MBC_Mixin.MBC_Cart with null record;

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

end Gade.Carts.MBC2;
