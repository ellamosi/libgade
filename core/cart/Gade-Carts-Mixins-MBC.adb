package body Gade.Carts.Mixins.MBC is

   overriding
   procedure Write_ROM
     (C       : in out MBC_Cart;
      Address : External_ROM_IO_Address;
      V       : Byte)
   is
   begin
      case Address is
         when RAM_Enable_Address  => C.Enable_RAM (Address, V);
         when Bank_Select_Address => C.Select_Bank (Address, V);
         when Special_Address     => C.Write_Special (V);
      end case;
   end Write_ROM;

end Gade.Carts.Mixins.MBC;
