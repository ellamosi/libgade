package body Gade.Carts.Banks.RAM is

   overriding
   procedure Write
     (B       : in out RAM_Bank;
      Address : Bank_Address;
      V       : Byte)
   is
   begin
      B.Content (Address + B.Offset) := V;
   end Write;

   overriding
   procedure Write
     (B       : in out Partial_RAM_Bank;
      Address : Bank_Address;
      V       : Byte)
   is
   begin
      B.Content (Memory_Content_Address (Address and B.Address_Mask)) := V;
   end Write;

end Gade.Carts.Banks.RAM;
