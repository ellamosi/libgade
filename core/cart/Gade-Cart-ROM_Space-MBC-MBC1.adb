with Gade.Cart.RAM; use Gade.Cart.RAM;
with Gade.Cart.ROM; use Gade.Cart.ROM;

package body Gade.Cart.ROM_Space.MBC.MBC1 is

   function Create
     (ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Handler : Gade.Cart.RAM_Space.RAM_Space_Access)
      return MBC1_ROM_Space_Access
   is
      Space : constant MBC1_ROM_Space_Access := new MBC1_ROM_Space_Type;
   begin
      MBC1.Initialize (Space.all, ROM_Content, RAM_Handler);
      return Space;
   end Create;

   procedure Initialize
     (Space       : out MBC1_ROM_Space_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Space   : Gade.Cart.RAM_Space.RAM_Space_Access)
   is
   begin
      MBC_ROM_Space_Type (Space).Initialize (ROM_Content, RAM_Space);
      Space.Reset;
   end Initialize;

   overriding
   procedure Reset (Space : in out MBC1_ROM_Space_Type) is
   begin
      Space.Banking_Mode := MBC1.ROM;
      Space.Low_Bank_Select := 1;
      Space.High_Bank_Select := 0;
      Space.Select_ROM_Bank;
      Space.Select_RAM_Bank;
   end Reset;

   overriding
   procedure Enable_RAM
     (Space   : in out MBC1_ROM_Space_Type;
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
         when Enable  => Space.RAM_Handler.Set_Enabled (True);
         when Disable => Space.RAM_Handler.Set_Enabled (False);
         when others  => null;
      end case;
   end Enable_RAM;

   overriding
   procedure Select_Bank
     (Space   : in out MBC1_ROM_Space_Type;
      Address : Bank_Select_Address;
      Value   : Byte)
   is
   begin
      case Address is
         when Low_Bank_Select_Address  => Space.Select_Low_Bank (Value);
         when High_Bank_Select_Address => Space.Select_High_Bank (Value);
      end case;
   end Select_Bank;

   overriding
   procedure Write_Special
     (Space : in out MBC1_ROM_Space_Type;
      Value : Byte)
   is
      New_Mode : constant Banking_Mode_Type :=
        Banking_Mode_Type'Val (Value and Banking_Mode_Mask);
   begin
      Space.Change_Banking_Mode (New_Mode);
   end Write_Special;

   procedure Change_Banking_Mode
     (Space    : in out MBC1_ROM_Space_Type;
      New_Mode : Banking_Mode_Type)
   is
      Mode_Changes : constant Boolean := New_Mode /= Space.Banking_Mode;
   begin
      if Mode_Changes then
         Space.Banking_Mode := New_Mode;
         Space.Select_ROM_Bank;
         Space.Select_RAM_Bank;
      end if;
   end Change_Banking_Mode;

   procedure Select_Low_Bank
     (Space : in out MBC1_ROM_Space_Type;
      Value : Byte)
   is
   begin
      Space.Low_Bank_Select := Low_Bank_Select_Type (Value and Low_Select_Mask);
      if Space.Low_Bank_Select = 0 then Space.Low_Bank_Select := 1; end if;
      Space.Select_ROM_Bank;
   end Select_Low_Bank;

   procedure Select_High_Bank
     (Space : in out MBC1_ROM_Space_Type;
      Value : Byte)
   is
   begin
      Space.High_Bank_Select := High_Bank_Select_Type (Value and High_Select_Mask);
      Space.Select_ROM_Bank;
      Space.Select_RAM_Bank;
   end Select_High_Bank;

   procedure Select_ROM_Bank
     (Space : in out MBC1_ROM_Space_Type)
   is
      Low_ROM_Bank_Number, High_ROM_Bank_Number : ROM_Bank_Range;
      Low_Part : constant Natural := Natural (Space.Low_Bank_Select);
      High_Part : constant Natural := Natural (Space.High_Bank_Select);
   begin
      High_ROM_Bank_Number := ROM_Bank_Range (High_Part * 2**5 + Low_Part);
      case Space.Banking_Mode is
         when ROM => Low_ROM_Bank_Number := 0;
         when RAM => Low_ROM_Bank_Number := ROM_Bank_Range (High_Part * 2**5);
      end case;
      Space.Switch_Banks (0, Low_ROM_Bank_Number);
      Space.Switch_Banks (1, High_ROM_Bank_Number);
   end Select_ROM_Bank;

   procedure Select_RAM_Bank
     (Space : in out MBC1_ROM_Space_Type)
   is
      Bank : RAM_Bank_Range;
   begin
      case Space.Banking_Mode is
         when ROM => Bank := 0;
         when RAM => Bank := RAM_Bank_Range (Space.High_Bank_Select);
      end case;
      Space.RAM_Handler.Switch_Banks (Bank);
   end Select_RAM_Bank;

end Gade.Cart.ROM_Space.MBC.MBC1;
