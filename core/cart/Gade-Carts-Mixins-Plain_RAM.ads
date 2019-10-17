private with Gade.Carts.Memory_Contents;

generic
   type Base_Cart is abstract new Cart with private;
package Gade.Carts.Mixins.Plain_RAM is

   type Plain_RAM_Cart is abstract new Base_Cart with private;

   overriding
   procedure Read_RAM
     (C       : in out Plain_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte);

   overriding
   procedure Write_RAM
     (C       : in out Plain_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte);

private
   use Gade.Carts.Memory_Contents;

   type Plain_RAM_Cart is abstract new Base_Cart with record
      RAM_Content : RAM_Content_Access;
   end record;

   Rebase_Mask : constant := 16#1FFF#;

   function Rebase
     (Address : External_RAM_IO_Address)
      return Memory_Content_Address;
   pragma Inline (Rebase);

end Gade.Carts.Mixins.Plain_RAM;
