package body Gade.Cart.Spaces.ROM.MBC is

   procedure Initialize
     (Space       : out MBC_ROM_Space_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Handler : RAM_Space_Access)
   is
   begin
      ROM_Space_Type (Space).Initialize (ROM_Content);
      Space.RAM_Handler := RAM_Handler;
   end Initialize;

   overriding
   procedure Write
     (Space   : in out MBC_ROM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
      pragma Unreferenced (GB);
   begin
      Space.ROM_Write (Address, Content);
   end Write;

   procedure ROM_Write
     (Space   : in out MBC_ROM_Space_Type'Class;
      Address : External_ROM_IO_Address;
      Content : Byte)
   is
   begin
      case Address is
         when RAM_Enable_Address  => Space.Enable_RAM (Address, Content);
         when Bank_Select_Address => Space.Select_Bank (Address, Content);
         when Special_Address     => Space.Write_Special (Content);
      end case;
   end ROM_Write;

   procedure Switch_Banks
     (Space            : in out MBC_ROM_Space_Type;
      Addressable_Bank : Addressable_Bank_Range;
      ROM_Bank         : ROM_Bank_Range)
   is
   begin
      Space.Addressable_Banks (Addressable_Bank).Set_Bank (ROM_Bank);
   end Switch_Banks;

end Gade.Cart.Spaces.ROM.MBC;
