package body Gade.Cart.C2.Mixins.Banked_ROM is
   use ROM_Banks;

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
            Read (C.Current_ROM_Banks (Bank0).all, Bank_Addr, V);
         when Bank1_Address =>
            Read (C.Current_ROM_Banks (Bank1).all, Bank_Addr, V);
      end case;
   end Read_ROM;

   procedure Select_ROM_Bank
     (C : in out Banked_ROM_Cart;
      L : ROM_Bank_Location;
      I : Bank_Index)
   is
   begin
      C.Current_ROM_Banks (L).Set_Bank (I);
   end Select_ROM_Bank;

end Gade.Cart.C2.Mixins.Banked_ROM;
