package body Gade.Carts.Banks.RAM is

   overriding
   procedure Write
     (B       : in out RAM_Bank;
      Address : Bank_Address;
      V       : Byte)
   is
   begin
      B.Content ((Address and B.Address_Mask) + B.Offset) := V;
   end Write;

end Gade.Carts.Banks.RAM;
