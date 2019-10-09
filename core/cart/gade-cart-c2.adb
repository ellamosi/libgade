package body Gade.Cart.C2 is

   procedure Read_ROM
     (C       : in out Cart;
      Address : External_ROM_IO_Address;
      V       : out Byte)
   is
      pragma Unreferenced (C, Address);
   begin
      V := Blank_Value;
   end Read_ROM;

   procedure Read_RAM
     (C       : in out Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte)
   is
      pragma Unreferenced (C, Address);
   begin
      V := Blank_Value;
   end Read_RAM;

end Gade.Cart.C2;
