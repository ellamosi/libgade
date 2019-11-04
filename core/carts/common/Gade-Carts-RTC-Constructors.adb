package body Gade.Carts.RTC.Constructors is

   function Create return Clock_NN_Access is
      Clk : Clock_Access;
   begin
      Clk := new Clock;
      Initialize (Clk.all);
      return Clk;
   end Create;

   procedure Initialize (Clk : out Clock) is
   begin
      Reset (Clk);
      Clk.Elapsed := Default_Counter_Values;
      Clk.Latched := Default_Counter_Values;
      To_Registers (Default_Counter_Values, Clk.Registers);
      Clk.Updated_At := Ada.Calendar.Clock;
   end Initialize;

end Gade.Carts.RTC.Constructors;
