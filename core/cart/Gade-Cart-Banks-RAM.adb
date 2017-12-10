package body Gade.Cart.Banks.RAM is

   procedure Save
     (File : RAM_Bank_IO.File_Type;
      Bank : RAM_Bank_Content_Type)
   is
      use RAM_Bank_IO;
   begin
      Write (File, Bank);
   end Save;

end Gade.Cart.Banks.RAM;
