package body Gade.Carts.Banks.RTC is

   overriding
   procedure Read
     (B       : in out RTC_Bank;
      Address : Bank_Address;
      V       : out Byte)
   is
      pragma Unreferenced (Address);
   begin
      Gade.Carts.RTC.Read (B.RTC.all, B.R, V);
   end Read;

   overriding
   procedure Write
     (B       : in out RTC_Bank;
      Address : Bank_Address;
      V       : Byte)
   is
      pragma Unreferenced (Address);
   begin
      Gade.Carts.RTC.Write (B.RTC.all, B.R, V);
   end Write;

end Gade.Carts.Banks.RTC;
