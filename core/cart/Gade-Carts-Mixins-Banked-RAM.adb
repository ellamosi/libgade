package body Gade.Carts.Mixins.Banked.RAM is

   overriding
   procedure Read_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte)
   is
   begin
      C.Accessible_Bank.Read (Decode (Address), V);
   end Read_RAM;

   overriding
   procedure Write_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte)
   is
   begin
      C.Accessible_Bank.Write (Decode (Address), V);
   end Write_RAM;

   procedure Select_RAM_Bank
     (C : in out Banked_RAM_Cart;
      I : Bank_Index)
   is
   begin
      C.Accessible_Index := I;
      if C.Enabled then C.Accessible_Bank := Select_Bank (C.Banks, I); end if;
   end Select_RAM_Bank;

   procedure Enable_RAM (C : in out Banked_RAM_Cart; Enable : Boolean) is
   begin
      C.Enabled := Enable;
      if Enable then
         C.Accessible_Bank := Select_Bank (C.Banks, C.Accessible_Index);
      else
         C.Accessible_Bank := Bank_Access (Blank_Banks.Singleton);
      end if;
   end Enable_RAM;

   function Is_RAM_Enabled (C : Banked_RAM_Cart) return Boolean is
   begin
      return C.Enabled;
   end Is_RAM_Enabled;

   function Decode (Address : External_RAM_IO_Address) return Bank_Address is
   begin
      return Address and Bank_Address_Mask;
   end Decode;

end Gade.Carts.Mixins.Banked.RAM;
