private with Gade.Cart.Memory_Contents;
private with Gade.Cart.C2.Mixins.Plain_RAM;

package Gade.Cart.C2.Plain is

   type Plain_Cart is new Cart with private;

   overriding
   procedure Read_ROM
     (C       : in out Plain_Cart;
      Address : External_ROM_IO_Address;
      V       : out Byte);

private
   use Gade.Cart.Memory_Contents;

   package Plain_RAM_Mixin is new Mixins.Plain_RAM (Cart);
   use Plain_RAM_Mixin;

   type Plain_Cart is new Plain_RAM_Cart with record
      ROM_Content : ROM_Content_Access;
   end record;

   function Rebase
     (Address : External_ROM_IO_Address)
      return Memory_Content_Address;
   pragma Inline (Rebase);

end Gade.Cart.C2.Plain;
