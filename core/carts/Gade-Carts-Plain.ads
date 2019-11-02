private with Gade.Carts.Mixins.Banked.ROM;
private with Gade.Carts.Mixins.Banked.RAM;

package Gade.Carts.Plain is

   type Plain_Cart is new Cart with private;

   type Plain_Cart_Access is access Plain_Cart'Class;

   subtype Plain_Cart_NN_Access is not null Plain_Cart_Access;

private

   package Plain_ROM_Mixin is new Gade.Carts.Mixins.Banked.ROM
     (Base_Cart        => Cart,
      Banks            => 1,
      Accessible_Banks => 1);
   package Plain_RAM_Mixin is new Gade.Carts.Mixins.Banked.RAM
     (Base_Cart          => Plain_ROM_Mixin.Banked_ROM_Cart,
      Banks              => 1,
      Enabled_By_Default => True);

   type Plain_Cart is new Plain_RAM_Mixin.Banked_RAM_Cart with null record;

end Gade.Carts.Plain;
