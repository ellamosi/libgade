package body Gade.Cart.Banks.ROM is

   procedure Load
     (File : ROM_Bank_IO.File_Type;
      Bank : out ROM_Bank_Access)
   is
      Buffer : constant Non_Constant_ROM_Bank_Access :=
        new ROM_Bank_Content_Type;
   begin
      ROM_Bank_IO.Read (File, Buffer.all);
      Bank := ROM_Bank_Access (Buffer);
   end Load;

end Gade.Cart.Banks.ROM;
