package body Gade.Carts.Mixins.Banked_RAM is

   overriding
   procedure Read_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte)
   is
   begin
      C.Accessible_Bank.Read (Rebase (Address), V);
   end Read_RAM;

   overriding
   procedure Write_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte)
   is
   begin
      C.Accessible_Bank.Write (Rebase (Address), V);
   end Write_RAM;

   procedure Select_RAM_Bank
     (C : in out Banked_RAM_Cart;
      I : Bank_Index)
   is
   begin
      C.Accessible_Bank := Select_Bank (C.Banks, I);
   end Select_RAM_Bank;

   function Rebase (Address : External_RAM_IO_Address) return Bank_Address is
   begin
      return Bank_Address (Address and Bank_Address_Mask);
   end Rebase;

end Gade.Carts.Mixins.Banked_RAM;
