package body Gade.Cart.Banks.RAM.Mem is

   overriding procedure Read
     (Handler : Memory_RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : out Byte)
   is
   begin
      Value := Handler.Content (Address);
   end Read;

   overriding procedure Write
     (Handler : in out Memory_RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : Byte)
   is
   begin
      Handler.Content (Address) := Value;
   end Write;

end Gade.Cart.Banks.RAM.Mem;
