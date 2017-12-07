package body Gade.Cartridge.MBC.MBC1 is

   overriding
   procedure Create
     (Handler : out MBC1_ROM_Handler_Type;
      ROM     : ROM_Access)
   is
   begin
      MBC_ROM_Handler_Type (Handler).Create (ROM);
      Handler.Banking_Mode := MBC1.ROM;
      Handler.Low_Bank_Select := 1;
      Handler.High_Bank_Select := 0;
   end Create;

   overriding
   procedure Select_Bank
     (Handler : in out MBC1_ROM_Handler_Type;
      Address : Bank_Select_Address;
      Value   : Byte)
   is
   begin
      case Address is
         when Low_Bank_Select_Address  => Handler.Select_Low_Bank (Value);
         when High_Bank_Select_Address => Handler.Select_High_Bank (Value);
      end case;
   end Select_Bank;

   overriding
   procedure Write_Special
     (Handler : in out MBC1_ROM_Handler_Type;
      Value   : Byte)
   is
      New_Banking_Mode : Banking_Mode_Type;
      Mode_Changed : Boolean;
   begin
      New_Banking_Mode := Banking_Mode_Type'Val (Value and Banking_Mode_Mask);
      Mode_Changed := New_Banking_Mode /= Handler.Banking_Mode;
      if Mode_Changed then
         Handler.Banking_Mode := New_Banking_Mode;
         case New_Banking_Mode is
            when ROM => Handler.Select_ROM_Bank;
            when RAM => Handler.Select_RAM_Bank;
         end case;
      end if;
   end Write_Special;

   procedure Select_Low_Bank
     (Handler : in out MBC1_ROM_Handler_Type;
      Value   : Byte)
   is
   begin
      Handler.Low_Bank_Select := Low_Bank_Select_Type (Value and Low_Select_Mask);
      if Handler.Low_Bank_Select = 0 then Handler.Low_Bank_Select := 1; end if;
      Handler.Select_ROM_Bank;
   end Select_Low_Bank;

   procedure Select_High_Bank
     (Handler : in out MBC1_ROM_Handler_Type;
      Value   : Byte)
   is
   begin
      Handler.High_Bank_Select := High_Bank_Select_Type (Value and High_Select_Mask);
      Handler.Select_ROM_Bank;
      Handler.Select_RAM_Bank;
   end Select_High_Bank;

   procedure Select_ROM_Bank
     (Handler : in out MBC1_ROM_Handler_Type)
   is
      Low_ROM_Bank_Number, High_ROM_Bank_Number : ROM_Bank_Range;
      Low_Part : constant Natural := Natural (Handler.Low_Bank_Select);
      High_Part : constant Natural := Natural (Handler.High_Bank_Select);
   begin
      case Handler.Banking_Mode is
         when ROM =>
            Low_ROM_Bank_Number := 0;
            High_ROM_Bank_Number := ROM_Bank_Range (High_Part * 2**5 + Low_Part);
         when RAM =>
            Low_ROM_Bank_Number := ROM_Bank_Range (High_Part * 2**5);
            High_ROM_Bank_Number := ROM_Bank_Range (Low_Part);
      end case;
      Handler.Set_ROM_Bank (0, Low_ROM_Bank_Number);
      Handler.Set_ROM_Bank (1, High_ROM_Bank_Number);
   end Select_ROM_Bank;

   procedure Select_RAM_Bank
     (Handler : in out MBC1_ROM_Handler_Type)
   is
      pragma Unreferenced (Handler);
   begin
      null;
      --  TODO
--        Gade.Dev.External_RAM.Switch_Banks
--          (Dummy,
--           External_RAM_Bank_Range (MBC.High_Bank_Select));
   end Select_RAM_Bank;

end Gade.Cartridge.MBC.MBC1;
