package body Gade.Carts.MBC2 is

   overriding
   procedure Read_RAM
     (C       : in out MBC2_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte)
   is
      Unmasked_V : Byte;
   begin
      Plain_RAM_Cart (C).Read_RAM (Address, Unmasked_V);
      V := Unmasked_V or RAM_Content_Mask;
   end Read_RAM;

   overriding
   procedure Write_RAM
     (C       : in out MBC2_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte)
   is
      Masked_V : constant Byte := V or RAM_Content_Mask;
   begin
      Plain_RAM_Cart (C).Write_RAM (Address, Masked_V);
   end Write_RAM;

   overriding
   procedure Enable_RAM
     (C       : in out MBC2_Cart;
      Address : RAM_Enable_Address;
      Value   : Byte)
   is
   begin
      if (Address and RAM_Enable_Accept_Mask) = 0 then
         case Value and RAM_Enable_Mask is
            when RAM_Enable_Value => C.Enable_RAM (True);
            when others           => C.Enable_RAM (False);
         end case;
      end if;
   end Enable_RAM;

   overriding
   procedure Select_Bank
     (C       : in out MBC2_Cart;
      Address : Bank_Select_Address;
      Value   : Byte)
   is
      Index : ROM_Bank_Index;
   begin
      if (Address and Bank_Select_Accept_Mask) /= 0 then
         Index := ROM_Bank_Index (Value and Bank_Select_Mask);
         C.Select_ROM_Bank (Bank1, Index);
      end if;
   end Select_Bank;

end Gade.Carts.MBC2;
