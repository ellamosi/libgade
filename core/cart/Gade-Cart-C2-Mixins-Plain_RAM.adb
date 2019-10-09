package body Gade.Cart.C2.Mixins.Plain_RAM is

   overriding
   procedure Read_RAM
     (C       : in out Plain_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte)
   is
   begin
      --  TODO: Ensure that the mod uses a power of 2 constant/bit mask
      V := C.RAM_Content (Rebase (Address) mod C.RAM_Content'Length);
   end Read_RAM;

   overriding
   procedure Write_RAM
     (C       : in out Plain_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte)
   is
   begin
      --  TODO: Ensure that the mod uses a power of 2 constant/bit mask
      C.RAM_Content (Rebase (Address) mod C.RAM_Content'Length) := V;
   end Write_RAM;

   function Rebase
     (Address : External_RAM_IO_Address) return Memory_Content_Address is
   begin
      return Memory_Content_Address (Address) and Rebase_Mask;
   end Rebase;

end Gade.Cart.C2.Mixins.Plain_RAM;

