package body Gade.Carts.Banks.RAM is

   overriding
   procedure Write
     (B       : in out RAM_Bank;
      Address : Bank_Address;
      V       : Byte)
   is
   begin
      B.Content (Decode (B, Address)) := V;
   end Write;

end Gade.Carts.Banks.RAM;
