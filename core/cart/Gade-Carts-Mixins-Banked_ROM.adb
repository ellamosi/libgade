package body Gade.Carts.Mixins.Banked_ROM is

   overriding
   procedure Read_ROM
     (C       : in out Banked_ROM_Cart;
      Address : External_ROM_IO_Address;
      V       : out Byte)
   is
      Bank_Addr : constant Bank_Address := Address and Bank_Address_Mask;
   begin
      case Address is
         when Bank0_Address =>
            Read (C.Accessible_Banks (Bank0).all, Bank_Addr, V);
         when Bank1_Address =>
            Read (C.Accessible_Banks (Bank1).all, Bank_Addr, V);
      end case;
   end Read_ROM;

   procedure Select_ROM_Bank
     (C : in out Banked_ROM_Cart;
      L : ROM_Bank_Location;
      I : Bank_Index)
   is
   begin
      C.Accessible_Banks (L) := ROM_Bank_Access (Select_Bank (C.Banks, I));
   end Select_ROM_Bank;

end Gade.Carts.Mixins.Banked_ROM;
