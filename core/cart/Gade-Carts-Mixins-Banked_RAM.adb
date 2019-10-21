package body Gade.Carts.Mixins.Banked_RAM is

   overriding
   procedure Read_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte)
   is
      B_Addr : constant Bank_Address := Rebase (Address, C.RAM_Address_Mask);
   begin
      C.Accessible_Bank.Read (B_Addr, V);
   end Read_RAM;

   overriding
   procedure Write_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte)
   is
      B_Addr : constant Bank_Address := Rebase (Address, C.RAM_Address_Mask);
   begin
      C.Accessible_Bank.Write (B_Addr, V);
   end Write_RAM;

   procedure Select_RAM_Bank
     (C : in out Banked_RAM_Cart;
      I : Bank_Index)
   is
   begin
      C.Accessible_Bank := C.Banks.Select_Bank (I);
   end Select_RAM_Bank;

   function Rebase
     (Address : External_RAM_IO_Address;
      Mask    : Word) return Bank_Address
   is
   begin
      return Bank_Address (Word (Address) and Mask);
   end Rebase;

end Gade.Carts.Mixins.Banked_RAM;
