package body Gade.Cart.C2.Plain is

   overriding
   procedure Read_ROM
     (C       : in out Plain_Cart;
      Address : External_ROM_IO_Address;
      V       : out Byte)
   is
   begin
      --  TODO: Ensure that the mod uses a power of 2 constant/bit mask
      V := C.ROM_Content (Rebase (Address) mod C.ROM_Content'Length);
   end Read_ROM;

   function Rebase
     (Address : External_ROM_IO_Address) return Memory_Content_Address is
   begin
      --  No need to do anything other than a simple cast, ROM addresses are
      --  already 0 based.
      return Memory_Content_Address (Address);
   end Rebase;

end Gade.Cart.C2.Plain;
