package body Gade.Cart.ROM.Handlers.MBC.MBC2 is

   function Create
     (ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Handler : Gade.Cart.RAM.Handlers.RAM_Handler_Access)
      return MBC2_ROM_Handler_Access
   is
      Handler : constant MBC2_ROM_Handler_Access := new MBC2_ROM_Handler_Type;
   begin
      MBC2.Initialize (Handler.all, ROM_Content, RAM_Handler);
      return Handler;
   end Create;

   procedure Initialize
     (Handler     : out MBC2_ROM_Handler_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Handler : Gade.Cart.RAM.Handlers.RAM_Handler_Access)
   is
   begin
      MBC_ROM_Handler_Type (Handler).Initialize (ROM_Content, RAM_Handler);
      Handler.Switch_Banks (0, 0);
      Handler.Reset;
   end Initialize;

   overriding
   procedure Reset
     (Handler : in out MBC2_ROM_Handler_Type)
   is
   begin
      Handler.Switch_Banks (0, 1);
   end Reset;

   overriding
   procedure Select_Bank
     (Handler : in out MBC2_ROM_Handler_Type;
      Address : Bank_Select_Address;
      Value   : Byte)
   is
      Bank_Index : ROM_Bank_Range;
   begin
      --  The least significant bit of the upper address byte must be one to
      --  select a ROM bank
      if (Address and Bank_Select_Flag_Mask) = Bank_Select_Flag_Mask then
         Bank_Index := ROM_Bank_Range (Value and Bank_Select_Index_Mask);
         Handler.Switch_Banks (1, Bank_Index);
      end if;
   end Select_Bank;

   overriding procedure Enable_RAM
     (Handler : in out MBC2_ROM_Handler_Type;
      Address : RAM_Enable_Address;
      Value   : Byte)
   is
   begin
      --  The least significant bit of the upper address byte must be zero to
      --  enable/disable cart RAM
      if (Address and RAM_Enable_Mask) = 0 then
         --  Handler.RAM_Handler.Toggle_Enabled;
         null;
      end if;
   end Enable_RAM;

end Gade.Cart.ROM.Handlers.MBC.MBC2;
