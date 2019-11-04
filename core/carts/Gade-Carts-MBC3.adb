with Gade.Carts.RTC.File_IO;

package body Gade.Carts.MBC3 is

   overriding
   procedure Reset (C : in out MBC3_Cart) is
   begin
      C.Reset_ROM;
      C.Reset_RAM;
      C.Last_Latch_Value := 16#01#;
      if C.RTC /= null then Reset (C.RTC.all); end if;
   end Reset;

   overriding
   procedure Load_RAM_File
     (C    : in out MBC3_Cart;
      File : Ada.Streams.Stream_IO.File_Type)
   is
      use Gade.Carts.RTC.File_IO;
   begin
      Banked_RAM_Mixin.Banked_RAM_Cart (C).Load_RAM_File (File);
      if C.RTC /= null then Load (C.RTC.all, File); end if;
   end Load_RAM_File;

   overriding
   procedure Save_RAM_File
     (C    : in out MBC3_Cart;
      File : Ada.Streams.Stream_IO.File_Type)
   is
      use Gade.Carts.RTC.File_IO;
   begin
      Banked_RAM_Mixin.Banked_RAM_Cart (C).Save_RAM_File (File);
      if C.RTC /= null then Save (C.RTC.all, File); end if;
   end Save_RAM_File;

   overriding
   procedure Enable_RAM
     (C       : in out MBC3_Cart;
      Address : RAM_Enable_Address;
      Value   : Byte)
   is
      pragma Unreferenced (Address);
   begin
      --  TODO: This could probably be shared with MBC1
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
      Latch_Sequence_Completed : constant Boolean :=
        C.Last_Latch_Value = 16#00# and Value = 16#01#;
   begin
      if C.RTC /= null and Latch_Sequence_Completed then
         Latch (C.RTC.all);
      end if;
      C.Last_Latch_Value := Value;
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
