generic
   type Base_Cart is abstract new Cart with private;
package Gade.Cart.C2.Mixins.Toggled_RAM is

   type Toggled_RAM_Cart is abstract new Base_Cart with private;

   overriding
   procedure Read_RAM
     (C       : in out Toggled_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte);

   overriding
   procedure Write_RAM
     (C       : in out Toggled_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte);

   procedure Enable_RAM
     (C      : in out Toggled_RAM_Cart;
      Enable : Boolean);

private

   type Toggled_RAM_Cart is abstract new Base_Cart with record
      RAM_Enabled : Boolean;
   end record;

end Gade.Cart.C2.Mixins.Toggled_RAM;
