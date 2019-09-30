package body Gade.Cart.Spaces.ROM.MBC is

   procedure Initialize
     (Handler     : out Handler_Type'Class;
      ROM_Content : Cart.ROM.Content_Access)
   is
   begin
      ROM.Handler_Type (Handler).Initialize (ROM_Content);
   end Initialize;

   overriding
   procedure Write
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
      pragma Unreferenced (GB);
   begin
      Handler.ROM_Write (Address, Content);
   end Write;

   procedure ROM_Write
     (Handler : in out Handler_Type'Class;
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
     (Handler          : in out Handler_Type;
      Addressable_Bank : Addressable_Bank_Range;
      Index            : Bank_Index)
   is
   begin
      Handler.Addressable_Banks (Addressable_Bank).Set_Bank (Index);
   end Switch_Banks;

end Gade.Cart.Spaces.ROM.MBC;
