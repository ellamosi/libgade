package body Gade.Carts.MBC2 is

   overriding
   procedure Write_ROM
     (C       : in out MBC2_Cart;
      Address : External_ROM_IO_Address;
      Value   : Byte)
   is
      Accepted_Address : constant Boolean := Address in Lower_ROM_IO_Address;
      RAM_Enable : constant Word := (Address and RAM_Enable_Accept_Mask);
   begin
      if Accepted_Address and RAM_Enable = 0 then
         C.Enable_RAM (Value);
      elsif Accepted_Address and RAM_Enable /= 0 then
         C.Select_Bank (Value);
      end if;
   end Write_ROM;

   procedure Select_Bank
     (C     : in out MBC2_Cart;
      Value : Byte)
   is
      use MBC_Mixin.ROM_RAM_Mixin.Banked_ROM_Mixin.Banked_ROM_Spaces;

      Requested_Index, Actual_Index : Bank_Index;
   begin
      Requested_Index := Bank_Index (Value and Bank_Select_Mask);
      Actual_Index := Bank_Index'Max (Requested_Index, 1);
      C.Select_ROM_Bank (1, Actual_Index);
   end Select_Bank;

   overriding
   procedure Finalize (C : in out MBC2_Cart) is
   begin
      MBC_Cart (C).Finalize;
      Cart (C).Finalize;
   end Finalize;

end Gade.Carts.MBC2;
