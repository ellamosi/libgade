with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Carts.Banks.RTC is

   overriding
   procedure Read
     (B       : in out RTC_Bank;
      Address : Bank_Address;
      V       : out Byte)
   is
      pragma Unreferenced (Address);
   begin
      Put_Line ("RTC.Read" & B.Register'Img);
      V := 0;
   end Read;

end Gade.Carts.Banks.RTC;
