package body Gade.Carts.Mixins.Banked.ROM is
   use Bank_Pools;

   procedure Reset_ROM (C : in out Banked_ROM_Cart) is
      I : Bank_Index;
   begin
      for AI in Accessible_Bank_Index loop
         I := Bank_Index (AI);
         C.Select_ROM_Bank (AI, I);
      end loop;
   end Reset_ROM;

   overriding
   procedure Read_ROM
     (C       : in out Banked_ROM_Cart;
      Address : External_ROM_IO_Address;
      V       : out Byte)
   is
      Bank_Addr : Bank_Address;
      Bank_Idx  : Accessible_Bank_Index;
   begin
      Decode (Address, Bank_Idx, Bank_Addr);
      Read (C.Accessible_Banks (Bank_Idx).all, Bank_Addr, V);
   end Read_ROM;

   procedure Select_ROM_Bank
     (C  : in out Banked_ROM_Cart;
      AI : Accessible_Bank_Index;
      I  : Bank_Index)
   is
   begin
      C.Accessible_Banks (AI) := ROM_Bank_Access (Select_Bank (C.Banks, I));
   end Select_ROM_Bank;

   procedure Decode
     (Address   : External_ROM_IO_Address;
      Bank_Idx  : out Accessible_Bank_Index;
      Bank_Addr : out Bank_Address)
   is
   begin
      Bank_Idx := Accessible_Bank_Index (Address / 2 ** Accessible_Index_Shift);
      Bank_Addr := Address and Bank_Address_Mask;
   end Decode;

end Gade.Carts.Mixins.Banked.ROM;
