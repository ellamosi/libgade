with Gade.Carts.RTC; use Gade.Carts.RTC;

generic
package Gade.Carts.Banks.RTC is

   type RTC_Bank is new Bank with private;

   type RTC_Bank_Access is access all RTC_Bank;

   subtype RTC_Bank_NN_Access is not null RTC_Bank_Access;

   overriding
   procedure Read
     (B       : in out RTC_Bank;
      Address : Bank_Address;
      V       : out Byte);

   overriding
   procedure Write
     (B       : in out RTC_Bank;
      Address : Bank_Address;
      V       : Byte);

private

   type RTC_Bank is new Bank with record
      RTC : Clock_Access;
      R   : Register;
   end record;

end Gade.Carts.Banks.RTC;
