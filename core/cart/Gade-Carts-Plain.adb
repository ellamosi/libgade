package body Gade.Carts.Plain is

   overriding
   procedure Read_ROM
     (C       : in out Plain_Cart;
      Address : External_ROM_IO_Address;
      V       : out Byte)
   is
   begin
      C.ROM.Read (Address, V);
   end Read_ROM;

   overriding
   procedure Read_RAM
     (C       : in out Plain_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte)
   is
   begin
      C.RAM.Read (Rebase (Address, C.Address_Mask), V);
   end Read_RAM;

   overriding
   procedure Write_RAM
     (C       : in out Plain_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte)
   is
   begin
      C.RAM.Write (Rebase (Address, C.Address_Mask), V);
   end Write_RAM;

   function Rebase
     (Address : External_RAM_IO_Address;
      Mask    : Word)
      return RAM_Space_Banks.Bank_Address
   is
   begin
      return RAM_Space_Banks.Bank_Address (Word (Address) and Mask);
   end Rebase;

end Gade.Carts.Plain;
