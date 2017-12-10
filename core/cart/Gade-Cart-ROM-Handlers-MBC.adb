package body Gade.Cart.ROM.Handlers.MBC is

   procedure Initialize
     (Handler     : out MBC_ROM_Handler_Type'Class;
      ROM_Content : ROM_Content_Access;
      RAM_Handler : RAM_Handler_Access)
   is
   begin
      ROM_Handler_Type (Handler).Initialize (ROM_Content);
      Handler.RAM_Handler := RAM_Handler;
   end Initialize;

   overriding
   procedure Write
     (Handler : in out MBC_ROM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
      pragma Unreferenced (GB);
   begin
      Handler.ROM_Write (Address, Content);
   end Write;

   procedure ROM_Write
     (Handler : in out MBC_ROM_Handler_Type'Class;
      Address : External_ROM_IO_Address;
      Content : Byte)
   is
   begin
      case Address is
         when RAM_Enable_Address  => Handler.Enable_RAM (Address, Content);
         when Bank_Select_Address => Handler.Select_Bank (Address, Content);
         when Special_Address     => Handler.Write_Special (Content);
      end case;
   end ROM_Write;

   procedure Switch_Banks
     (Handler          : in out MBC_ROM_Handler_Type;
      Addressable_Bank : Addressable_Bank_Range;
      ROM_Bank         : ROM_Bank_Range)
   is
      --  If the ROM has fewer banks than supported the higher pins won't be
      --  wired, therefore we are going to be looping banks if selecting
      --  a higher bank than available.
      Bank : constant ROM_Bank_Range := ROM_Bank mod Handler.ROM_Content'Length;
   begin
      Handler.Addressable_Banks (Addressable_Bank) := Handler.ROM_Content (Bank);
   end Switch_Banks;

end Gade.Cart.ROM.Handlers.MBC;
