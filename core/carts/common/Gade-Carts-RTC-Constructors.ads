package Gade.Carts.RTC.Constructors is

   function Create return Clock_NN_Access;

private

   procedure Initialize (Clk : out Clock);

   --  For whenever we are not loading from a savefile.
   --  The sensible values for an out of factory cart are unknown.
   Default_Counter_Values : constant Counter :=
     (Span   => 0.0,
      Halted => True,
      Carry  => False);

end Gade.Carts.RTC.Constructors;
