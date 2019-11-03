with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Carts.MBC3 is

   overriding
   procedure Reset (C : in out MBC3_Cart) is
   begin
      C.Reset_ROM;
      C.Reset_RAM;
      --  TODO: Reset RTC (without clearing actual time data)
   end Reset;

   overriding
   procedure Enable_RAM
     (C       : in out MBC3_Cart;
      Address : RAM_Enable_Address;
      Value   : Byte)
   is
      pragma Unreferenced (Address);
   begin
      case Value and RAM_Enable_Mask is
         when RAM_Enable_Value => C.Enable_RAM (True);
         when others           => C.Enable_RAM (False);
      end case;
   end Enable_RAM;

   overriding
   procedure Write_Special
     (C       : in out MBC3_Cart;
      Value   : Byte)
   is
      pragma Unreferenced (Value);
   begin
      if C.RTC /= null then
         Put_Line ("Latch");
         Latch (C.RTC.all);
      end if;
   end Write_Special;

   overriding
   procedure Select_Bank
     (C       : in out MBC3_Cart;
      Address : Bank_Select_Address;
      Value   : Byte)
   is
   begin
      if Address in ROM_Bank_Select_Address then
         C.Select_ROM_Bank (Value);
      elsif Address in RAM_Bank_Select_Address then
         C.Select_RAM_Bank (Value);
      end if;
   end Select_Bank;

   procedure Select_ROM_Bank (C : in out MBC3_Cart; Value : Byte) is
      use Banked_ROM_Mixin.Banked_ROM_Spaces;
   begin
      C.Select_ROM_Bank (1, Bank_Index (Value and ROM_Index_Mask));
   end Select_ROM_Bank;

   procedure Select_RAM_Bank (C : in out MBC3_Cart; Value : Byte) is
      use Banked_RAM_Mixin.Banked_RAM_Spaces;
   begin
      C.Select_RAM_Bank (Bank_Index (Value and RAM_Index_Mask));
   end Select_RAM_Bank;

end Gade.Carts.MBC3;
