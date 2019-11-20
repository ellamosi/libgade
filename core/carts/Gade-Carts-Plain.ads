private with Gade.Carts.Mixins.ROM_RAM;

package Gade.Carts.Plain is

   type Plain_Cart is new Cart with private;

   type Plain_Cart_Access is access Plain_Cart'Class;

   subtype Plain_Cart_NN_Access is not null Plain_Cart_Access;

   overriding
   procedure Reset (C : in out Plain_Cart);

private

   package ROM_RAM_Mixin is new Gade.Carts.Mixins.ROM_RAM
     (Base_Cart              => Cart,
      ROM_Banks              => 1,
      RAM_Banks              => 1,
      Accessible_ROM_Banks   => 1,
      RAM_Enabled_By_Default => True);
   use ROM_RAM_Mixin;

   type Plain_Cart is new ROM_RAM_Cart with null record;

end Gade.Carts.Plain;
