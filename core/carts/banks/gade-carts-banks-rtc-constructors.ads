generic
package Gade.Carts.Banks.RTC.Constructors is

   function Create
     (RTC : Clock_Access;
      R   : Register)
      return RTC_Bank_NN_Access;

   procedure Initialize
     (B   : out RTC_Bank'Class;
      RTC : Clock_Access;
      R   : Register);

end Gade.Carts.Banks.RTC.Constructors;
