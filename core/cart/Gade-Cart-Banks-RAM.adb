package body Gade.Cart.Banks.RAM is

   procedure Initialize (Bank : out RAM_Bank_Content_Type) is
   begin
      Bank := (others => 0);
   end Initialize;

   function Load
     (File : RAM_Bank_IO.File_Type) return RAM_Bank_Content_Access
   is
      Bank : constant RAM_Bank_Content_Access := new RAM_Bank_Content_Type;
   begin
      RAM_Bank_IO.Read (File, Bank.all);
      return Bank;
   end Load;

   procedure Save
     (File : RAM_Bank_IO.File_Type;
      Bank : RAM_Bank_Content_Type)
   is
   begin
      RAM_Bank_IO.Write (File, Bank);
   end Save;

end Gade.Cart.Banks.RAM;
