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

   procedure Enable_RAM
     (C       : in out MBC_Cart;
      Address : RAM_Enable_Address;
      V       : Byte)
   is
      pragma Unreferenced (Address);
   begin
      case V and RAM_Enable_Mask is
         when RAM_Enable_Value => C.Enable_RAM (True);
         when others           => C.Enable_RAM (False);
      end case;
   end Enable_RAM;

end Gade.Carts.Mixins.MBC;
