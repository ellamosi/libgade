with Gade.Cart.RAM; use Gade.Cart.RAM;
with Gade.Cart.ROM; use Gade.Cart.ROM;

package body Gade.Cart.Spaces.ROM.MBC.MBC1 is

   function Create
     (ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Handler : Gade.Cart.Spaces.RAM.RAM_Space_Access)
      return Handler_Access
   is
      Handler : constant Handler_Access := new Handler_Type;
   begin
      MBC1.Initialize (Handler.all, ROM_Content, RAM_Handler);
      return Handler;
   end Create;

   procedure Initialize
     (Handler     : out Handler_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Space   : Gade.Cart.Spaces.RAM.RAM_Space_Access)
   is
   begin
      MBC.Handler_Type (Handler).Initialize (ROM_Content, RAM_Space);
      Handler.Reset;
   end Initialize;

   overriding
   procedure Reset (Handler : in out Handler_Type) is
   begin
      Handler.Banking_Mode := MBC1.ROM;
      Handler.Low_Bank_Select := 1;
      Handler.High_Bank_Select := 0;
      Handler.Select_ROM_Bank;
      Handler.Select_RAM_Bank;
   end Reset;

   overriding
   procedure Enable_RAM
     (Handler : in out Handler_Type;
      Address : RAM_Enable_Address;
      Value   : Byte)
   is
      pragma Unreferenced (Address);
      Mask    : constant := 16#0F#;
      Enable  : constant := 16#0A#;
      Disable : constant := 16#00#;

      Masked_Value : constant Byte := Value and Mask;
   begin
      case Masked_Value is
         when Enable  => Handler.RAM_Handler.Set_Enabled (True);
         when Disable => Handler.RAM_Handler.Set_Enabled (False);
         when others  => null;
      end case;
   end Enable_RAM;

   overriding
   procedure Select_Bank
     (Handler : in out Handler_Type;
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
     (Handler : in out Handler_Type;
      Value   : Byte)
   is
      New_Mode : constant Banking_Mode_Type :=
        Banking_Mode_Type'Val (Value and Banking_Mode_Mask);
   begin
      Handler.Change_Banking_Mode (New_Mode);
   end Write_Special;

   procedure Change_Banking_Mode
     (Handler  : in out Handler_Type;
      New_Mode : Banking_Mode_Type)
   is
      Mode_Changes : constant Boolean := New_Mode /= Handler.Banking_Mode;
   begin
      if Mode_Changes then
         Handler.Banking_Mode := New_Mode;
         Handler.Select_ROM_Bank;
         Handler.Select_RAM_Bank;
      end if;
   end Change_Banking_Mode;

   procedure Select_Low_Bank
     (Handler : in out Handler_Type;
      Value   : Byte)
   is
   begin
      Handler.Low_Bank_Select := Low_Bank_Select_Type (Value and Low_Select_Mask);
      if Handler.Low_Bank_Select = 0 then Handler.Low_Bank_Select := 1; end if;
      Handler.Select_ROM_Bank;
   end Select_Low_Bank;

   procedure Select_High_Bank
     (Handler : in out Handler_Type;
      Value   : Byte)
   is
   begin
      Handler.High_Bank_Select := High_Bank_Select_Type (Value and High_Select_Mask);
      Handler.Select_ROM_Bank;
      Handler.Select_RAM_Bank;
   end Select_High_Bank;

   procedure Select_ROM_Bank (Handler : in out Handler_Type) is
      Low_ROM_Bank_Number, High_ROM_Bank_Number : ROM_Bank_Range;
      Low_Part : constant Natural := Natural (Handler.Low_Bank_Select);
      High_Part : constant Natural := Natural (Handler.High_Bank_Select);
   begin
      High_ROM_Bank_Number := ROM_Bank_Range (High_Part * 2**5 + Low_Part);
      case Handler.Banking_Mode is
         when ROM => Low_ROM_Bank_Number := 0;
         when RAM => Low_ROM_Bank_Number := ROM_Bank_Range (High_Part * 2**5);
      end case;
      Handler.Switch_Banks (0, Low_ROM_Bank_Number);
      Handler.Switch_Banks (1, High_ROM_Bank_Number);
   end Select_ROM_Bank;

   procedure Select_RAM_Bank (Handler : in out Handler_Type) is
      Bank : RAM_Bank_Range;
   begin
      case Handler.Banking_Mode is
         when ROM => Bank := 0;
         when RAM => Bank := RAM_Bank_Range (Handler.High_Bank_Select);
      end case;
      Handler.RAM_Handler.Switch_Banks (Bank);
   end Select_RAM_Bank;

end Gade.Cart.Spaces.ROM.MBC.MBC1;

