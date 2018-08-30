package body Gade.Cart.Banks.RAM is

   procedure Initialize (Bank : out RAM_Bank_Content_Type) is
   begin
      Bank := (others => 0);
   end Initialize;

end Gade.Cart.Banks.RAM;
