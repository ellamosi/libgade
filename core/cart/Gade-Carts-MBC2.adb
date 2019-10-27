package body Gade.Carts.MBC2 is

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
         C.Select_ROM_Bank (1, Index);
      end if;
   end Select_Bank;

end Gade.Carts.MBC2;
