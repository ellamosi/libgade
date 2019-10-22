with Ada.Text_IO; use Ada.Text_IO;

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
      C.Accessible_Index := I;
      if C.Enabled then C.Accessible_Bank := Select_Bank (C.Banks, I); end if;
   end Select_RAM_Bank;

   procedure Enable_RAM (C : in out Banked_RAM_Cart; Enable : Boolean) is
   begin
      Put_Line ("Enable_RAM: " & Enable'Img);
      C.Enabled := Enable;
      if Enable then
         C.Accessible_Bank := Select_Bank (C.Banks, C.Accessible_Index);
      else
         C.Accessible_Bank := Bank_Access (Blank_RAM_Banks.Singleton);
      end if;
   end Enable_RAM;

   function Rebase (Address : External_RAM_IO_Address) return Bank_Address is
   begin
      return Bank_Address (Address and Bank_Address_Mask);
   end Rebase;

end Gade.Carts.Mixins.Banked_RAM;
