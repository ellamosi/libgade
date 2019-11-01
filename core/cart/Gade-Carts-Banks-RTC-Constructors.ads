generic
package Gade.Carts.Banks.RTC.Constructors is

   function Create
     (RTC      : Clock_Access;
      Register : Clock_Register)
      return RTC_Bank_NN_Access;

   procedure Initialize
     (B        : out RTC_Bank'Class;
      RTC      : Clock_Access;
      Register : Clock_Register);

end Gade.Carts.Banks.RTC.Constructors;
