package body Gade.Carts.Banks.RTC.Constructors is

   function Create
     (RTC : Clock_Access;
      R   : Register)
      return RTC_Bank_NN_Access
   is
      Result : constant RTC_Bank_NN_Access := new RTC_Bank;
   begin
      Constructors.Initialize (Result.all, RTC, R);
      return Result;
   end Create;

   procedure Initialize
     (B   : out RTC_Bank'Class;
      RTC : Clock_Access;
      R   : Register)
   is
   begin
      B.RTC := RTC;
      B.R := R;
   end Initialize;

end Gade.Carts.Banks.RTC.Constructors;
