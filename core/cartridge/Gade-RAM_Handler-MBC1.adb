package body Gade.RAM_Handler.MBC1 is

   overriding
   procedure Create
     (Handler : out MBC1_RAM_Handler_Type)
   is
   begin
      Handler.RAM := new RAM_Type;
      for Bank of Handler.RAM.all loop
         Bank := new RAM_Bank_Type;
      end loop;
      Handler.Switch_Banks (0);
      Handler.Enabled := False;
   end Create;

   overriding
   procedure Read
     (Handler : in out MBC1_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (GB);
   begin
      if Handler.Enabled then
         Content := Handler.Bank (Address and Bank_Address_Mask);
      else
         Content := 16#FF#;
      end if;
   end Read;

   overriding
   procedure Write
     (Handler : in out MBC1_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
      pragma Unreferenced (GB);
   begin
      if Handler.Enabled then
         Handler.Bank (Address and Bank_Address_Mask) := Content;
      end if;
   end Write;

   overriding
   procedure Switch_Banks
     (Handler : in out MBC1_RAM_Handler_Type;
      Bank    : RAM_Bank_Range)
   is
   begin
      Handler.Bank := Handler.RAM (Bank);
   end Switch_Banks;

   overriding
   procedure Set_Enabled
     (Handler : in out MBC1_RAM_Handler_Type;
      Enabled : Boolean)
   is
   begin
      Handler.Enabled := Enabled;
   end Set_Enabled;

end Gade.RAM_Handler.MBC1;
