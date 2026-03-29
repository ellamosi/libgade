package body Gade.Carts.MBC5 is
   use MBC_Mixin.ROM_RAM_Mixin;

   overriding
   procedure Reset (C : in out MBC5_Cart) is
   begin
      MBC_Cart (C).Reset;
      C.Low_ROM_Bank_Select := 1;
      C.High_ROM_Bank_Select := 0;
      C.RAM_Bank_Select := 0;
      C.Rumble_Enabled := False;
   end Reset;

   overriding
   procedure Select_Bank
     (C : in out MBC5_Cart; Address : Bank_Select_Address; Value : Byte) is
   begin
      case Address is
         when Low_ROM_Bank_Select_Address  =>
            C.Select_Low_ROM_Bank (Value);

         when High_ROM_Bank_Select_Address =>
            C.Select_High_ROM_Bank (Value);

         when RAM_Bank_Select_Address      =>
            C.Select_RAM_Bank (Value);
      end case;
   end Select_Bank;

   procedure Select_Low_ROM_Bank (C : in out MBC5_Cart; Value : Byte) is
   begin
      C.Low_ROM_Bank_Select := Low_ROM_Bank_Select_Type (Value and Low_ROM_Index_Mask);
      C.Select_ROM_Bank;
   end Select_Low_ROM_Bank;

   procedure Select_High_ROM_Bank (C : in out MBC5_Cart; Value : Byte) is
   begin
      C.High_ROM_Bank_Select := High_ROM_Bank_Select_Type (Value and High_ROM_Index_Mask);
      C.Select_ROM_Bank;
   end Select_High_ROM_Bank;

   procedure Select_ROM_Bank (C : in out MBC5_Cart) is
      use Banked_ROM_Mixin.Banked_ROM_Spaces;

      Index : constant Bank_Index :=
        Bank_Index
          (Natural (C.High_ROM_Bank_Select) * 2**8 + Natural (C.Low_ROM_Bank_Select));
   begin
      C.Select_ROM_Bank (1, Index);
   end Select_ROM_Bank;

   procedure Select_RAM_Bank (C : in out MBC5_Cart; Value : Byte) is
      use Banked_RAM_Mixin.Banked_RAM_Spaces;

      Mask : constant Byte :=
        (if C.Has_Rumble then Rumble_RAM_Index_Mask else RAM_Index_Mask);
   begin
      C.RAM_Bank_Select := RAM_Bank_Select_Type (Value and Mask);
      C.Rumble_Enabled := C.Has_Rumble and then (Value and Rumble_Bit_Mask) /= 0;
      C.Select_RAM_Bank (Bank_Index (Natural (C.RAM_Bank_Select)));
   end Select_RAM_Bank;

end Gade.Carts.MBC5;
