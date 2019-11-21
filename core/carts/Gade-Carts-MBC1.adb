package body Gade.Carts.MBC1 is
   use MBC_Mixin.ROM_RAM_Mixin;

   overriding
   procedure Reset (C : in out MBC1_Cart) is
   begin
      C.Reset_ROM;
      C.Reset_RAM;
      C.Low_Bank_Select := 1;
      C.High_Bank_Select := 0;
      C.Banking_Mode := ROM;
   end Reset;

   overriding
   procedure Write_Special
     (C       : in out MBC1_Cart;
      Value   : Byte)
   is
      New_Mode : constant Banking_Mode_Type :=
        Banking_Mode_Type'Val (Value and Banking_Mode_Mask);
   begin
      C.Change_Banking_Mode (New_Mode);
   end Write_Special;

   procedure Change_Banking_Mode
     (C        : in out MBC1_Cart;
      New_Mode : Banking_Mode_Type)
   is
      Mode_Changes : constant Boolean := New_Mode /= C.Banking_Mode;
   begin
      if Mode_Changes then
         C.Banking_Mode := New_Mode;
         C.Select_ROM_Bank;
         C.Select_RAM_Bank;
      end if;
   end Change_Banking_Mode;

   overriding
   procedure Select_Bank
     (C       : in out MBC1_Cart;
      Address : Bank_Select_Address;
      Value   : Byte)
   is
   begin
      case Address is
         when Low_Bank_Select_Address  => C.Select_Low_Bank (Value);
         when High_Bank_Select_Address => C.Select_High_Bank (Value);
      end case;
   end Select_Bank;

   procedure Select_Low_Bank
     (C     : in out MBC1_Cart;
      Value : Byte)
   is
   begin
      C.Low_Bank_Select := Low_Bank_Select_Type (Value and Low_Select_Mask);
      if C.Low_Bank_Select = 0 then C.Low_Bank_Select := 1; end if;
      C.Select_ROM_Bank;
   end Select_Low_Bank;

   procedure Select_High_Bank
     (C     : in out MBC1_Cart;
      Value : Byte)
   is
   begin
      C.High_Bank_Select := High_Bank_Select_Type (Value and High_Select_Mask);
      C.Select_ROM_Bank;
      C.Select_RAM_Bank;
   end Select_High_Bank;

   procedure Select_ROM_Bank (C : in out MBC1_Cart) is
      use Banked_ROM_Mixin.Banked_ROM_Spaces;

      Low_Bank_Index, High_Bank_Index : Bank_Index;
      Low_Part : constant Natural := Natural (C.Low_Bank_Select);
      High_Part : constant Natural := Natural (C.High_Bank_Select);
   begin
      High_Bank_Index := Bank_Index (High_Part * 2**5 + Low_Part);
      case C.Banking_Mode is
         when ROM => Low_Bank_Index := 0;
         when RAM => Low_Bank_Index := Bank_Index (High_Part * 2**5);
      end case;
      C.Select_ROM_Bank (0, Low_Bank_Index);
      C.Select_ROM_Bank (1, High_Bank_Index);
   end Select_ROM_Bank;

   procedure Select_RAM_Bank (C : in out MBC1_Cart) is
      use Banked_RAM_Mixin.Banked_RAM_Spaces;

      Index : Bank_Index;
   begin
      case C.Banking_Mode is
         when ROM => Index := 0;
         when RAM => Index := Bank_Index (C.High_Bank_Select);
      end case;
      C.Select_RAM_Bank (Index);
   end Select_RAM_Bank;

end Gade.Carts.MBC1;