with Gade.Carts.Mixins.Banked.ROM;
with Gade.Carts.Mixins.Banked.RAM;

--  This package is just a shortcut to instantiate both ROM and RAM mixins in
--  a single go and simplifying declarations. All types of carts may potentially
--  support ROM+RAM in one way or another.

generic
   type Base_Cart is abstract new Cart with private;
   ROM_Banks, RAM_Banks   : in Positive;
   Accessible_ROM_Banks   : in Positive := 2;
   RAM_Enabled_By_Default : in Boolean := False;
package Gade.Carts.Mixins.ROM_RAM is

   package Banked_ROM_Mixin is new Gade.Carts.Mixins.Banked.ROM
     (Base_Cart        => Base_Cart,
      Banks            => ROM_Banks,
      Accessible_Banks => Accessible_ROM_Banks);
   package Banked_RAM_Mixin is new Gade.Carts.Mixins.Banked.RAM
     (Base_Cart          => Banked_ROM_Mixin.Banked_ROM_Cart,
      Banks              => RAM_Banks,
      Enabled_By_Default => RAM_Enabled_By_Default);
   use Banked_RAM_Mixin;

   type ROM_RAM_Cart is new Banked_RAM_Cart with private;

private

   type ROM_RAM_Cart is new Banked_RAM_Cart with null record;

end Gade.Carts.Mixins.ROM_RAM;
