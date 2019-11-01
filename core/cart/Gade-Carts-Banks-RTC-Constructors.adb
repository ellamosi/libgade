with Ada.Text_IO; use Ada.Text_IO;
package body Gade.Carts.Banks.RTC.Constructors is

   function Create
     (RTC      : Clock_Access;
      Register : Clock_Register)
      return RTC_Bank_NN_Access
   is
      Result : constant RTC_Bank_NN_Access := new RTC_Bank;
   begin
      Constructors.Initialize (Result.all, RTC, Register);
      return Result;
   end Create;

   procedure Initialize
     (B        : out RTC_Bank'Class;
      RTC      : Clock_Access;
      Register : Clock_Register)
   is
   begin
      Put_Line ("Created RTC" & Register'Img & " Bank");
      B.RTC := RTC;
      B.Register := Register;
   end Initialize;

end Gade.Carts.Banks.RTC.Constructors;
