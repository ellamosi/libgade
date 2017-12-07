package body Gade.Cartridge.MBC is

   overriding
   procedure Create
     (Handler : out MBC_ROM_Handler_Type;
      ROM     : ROM_Access)
   is
   begin
      ROM_Only_Handler_Type (Handler).Create (ROM);
      Handler.RAM_Enabled := False;
      --  TODO: Assign RAM handler
   end Create;

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

end Gade.Cartridge.MBC;
