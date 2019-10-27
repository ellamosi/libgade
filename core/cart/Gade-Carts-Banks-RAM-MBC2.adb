package body Gade.Carts.Banks.RAM.MBC2 is

   overriding
   procedure Read
     (B       : in out MBC2_RAM_Bank;
      Address : Bank_Address;
      V       : out Byte)
   is
   begin
      RAM_Bank (B).Read (Address, V);
      V := V and Value_Mask;
   end Read;

   overriding
   procedure Write
     (B       : in out MBC2_RAM_Bank;
      Address : Bank_Address;
      V       : Byte)
   is
   begin
      RAM_Bank (B).Write (Address, V and Value_Mask);
   end Write;

end Gade.Carts.Banks.RAM.MBC2;
