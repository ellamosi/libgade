package body Gade.Cart.Banks.ROM is

   function Load (File : ROM_Bank_IO.File_Type) return ROM_Bank_Access is
      Bank : constant Non_Constant_ROM_Bank_Access := new ROM_Bank_Content_Type;
   begin
      ROM_Bank_IO.Read (File, Bank.all);
      return ROM_Bank_Access (Bank);
   end Load;

end Gade.Cart.Banks.ROM;
