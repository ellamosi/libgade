package body Gade.Cart.C2.Mixins.Banked_RAM is

   overriding
   procedure Read_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte)
   is
   begin
      C.Current_RAM_Bank.Read (Address - External_RAM_IO_Address'First, V);
   end Read_RAM;

   overriding
   procedure Write_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte)
   is
   begin
      C.Current_RAM_Bank.Write (Address - External_RAM_IO_Address'First, V);
   end Write_RAM;

   procedure Select_RAM_Bank
     (C : in out Banked_RAM_Cart;
      I : Bank_Index)
   is
   begin
      C.Current_RAM_Bank.Set_Bank (I);
   end Select_RAM_Bank;

end Gade.Cart.C2.Mixins.Banked_RAM;
