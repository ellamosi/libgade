package body Gade.Carts.Mixins.MBC is

   overriding
   procedure Write_ROM
     (C       : in out MBC_Cart;
      Address : External_ROM_IO_Address;
      V       : Byte)
   is
   begin
      case Address is
         when RAM_Enable_Address  =>
            Enable_RAM (MBC_Cart'Class (C), Address, V);
         when Bank_Select_Address =>
            Select_Bank (MBC_Cart'Class (C), Address, V);
         when Special_Address     =>
            Write_Special (MBC_Cart'Class (C), V);
      end case;
   end Write_ROM;

end Gade.Carts.Mixins.MBC;
