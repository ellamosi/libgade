package body Gade.Carts.MBC2 is

   overriding
   procedure Reset (C : in out MBC2_Cart) is
   begin
      C.Reset_ROM;
      C.Reset_RAM;
   end Reset;

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

   procedure Enable_RAM
     (C     : in out MBC2_Cart;
      Value : Byte)
   is
   begin
      case Value and RAM_Enable_Mask is
         when RAM_Enable_Value => C.Enable_RAM (True);
         when others           => C.Enable_RAM (False);
      end case;
   end Enable_RAM;

   procedure Select_Bank
     (C     : in out MBC2_Cart;
      Value : Byte)
   is
      use Banked_ROM_Mixin.Banked_ROM_Spaces;
      Index : Bank_Index;
   begin
      Index := Bank_Index (Value and Bank_Select_Mask);
      if Index = 0 then Index := 1; end if;
      C.Select_ROM_Bank (1, Index);
   end Select_Bank;

end Gade.Carts.MBC2;
