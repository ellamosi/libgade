with Gade.Cart.ROM; use Gade.Cart.ROM;
with Gade.Cart.RAM; use Gade.Cart.RAM;

package body Gade.Cart.Spaces.ROM.MBC.MBC3 is

   function Create
     (ROM_Content : Cart.ROM.Content_Access;
      RAM_Handler : Spaces.RAM.Handler_Access)
      return Handler_Access
   is
      Handler : constant Handler_Access := new Handler_Type;
   begin
      MBC3.Initialize (Handler.all, ROM_Content, RAM_Handler);
      return Handler;
   end Create;

   procedure Initialize
     (Handler     : out Handler_Type'Class;
      ROM_Content : Cart.ROM.Content_Access;
      RAM_Handler : Spaces.RAM.Handler_Access)
   is
   begin
      MBC.Handler_Type (Handler).Initialize (ROM_Content, RAM_Handler);
      Handler.Reset;
   end Initialize;

   overriding
   procedure Reset (Handler : in out Handler_Type) is
   begin
      Handler.Addressable_Banks (1).Set_Bank (1);
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
   begin
      Handler.RAM_Handler.Set_Enabled ((Value and Mask) = Enable);
   end Enable_RAM;

   overriding
   procedure Select_Bank
     (Handler : in out Handler_Type;
      Address : Bank_Select_Address;
      Value   : Byte)
   is
   begin
      case Address is
         when ROM_Bank_Select_Address => Handler.Select_ROM_Bank (Value);
         when RAM_Bank_Select_Address => Handler.Select_RAM_Bank (Value);
      end case;
   end Select_Bank;

   overriding
   procedure Write_Special
     (Handler : in out Handler_Type;
      Value   : Byte)
   is
   begin
      --  Latch Clock Data
      null;
   end Write_Special;

   procedure Select_ROM_Bank
     (Handler : in out Handler_Type;
      Value   : Byte)
   is
      Bank_Mask : constant Byte := 16#7F#;
      Index : Cart.ROM.Bank_Index;
   begin
      Index := Cart.ROM.Bank_Index (Value and Bank_Mask);
      if Index = 0 then Index := 1; end if;
      Handler.Addressable_Banks (1).Set_Bank (Index);
   end Select_ROM_Bank;

   procedure Select_RAM_Bank
     (Handler : in out Handler_Type;
      Value   : Byte)
   is
      --  TODO : This does not support selecting the RTC
      Bank_Mask : constant Byte := 16#07#;
      Index : Cart.RAM.Bank_Index;
   begin
      Index := Cart.RAM.Bank_Index (Value and Bank_Mask);
      Handler.RAM_Handler.Switch_Banks (Index);
   end Select_RAM_Bank;

end Gade.Cart.Spaces.ROM.MBC.MBC3;
