with Gade.Carts.RTC; use Gade.Carts.RTC;

generic
package Gade.Carts.Banks.RTC is

   --  type Clock_Register is (Seconds, Minutes, Hours, Days_Low, Days_High);

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

--     type Seconds_RTC_Bank is new RTC_Bank with null record;
--
--     overriding
--     procedure Read
--       (B       : in out Seconds_RTC_Bank;
--        Address : Bank_Address;
--        V       : out Byte);
--
--     type Minutes_RTC_Bank is new RTC_Bank with null record;
--
--     overriding
--     procedure Read
--       (B       : in out Minutes_RTC_Bank;
--        Address : Bank_Address;
--        V       : out Byte);
--
--     type Hours_RTC_Bank is new RTC_Bank with null record;
--
--     overriding
--     procedure Read
--       (B       : in out Hours_RTC_Bank;
--        Address : Bank_Address;
--        V       : out Byte);
--
--     type Days_Low_RTC_Bank is new RTC_Bank with null record;
--
--     overriding
--     procedure Read
--       (B       : in out Days_Low_RTC_Bank;
--        Address : Bank_Address;
--        V       : out Byte);
--
--     type Days_High_RTC_Bank is new RTC_Bank with null record;
--
--     overriding
--     procedure Read
--       (B       : in out Days_High_RTC_Bank;
--        Address : Bank_Address;
--        V       : out Byte);

end Gade.Carts.Banks.RTC;
